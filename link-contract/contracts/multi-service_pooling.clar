;; Multi-Service Pooling Smart Contract
;; Allows users to pool funds for multiple subscription services

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-POOL-NOT-FOUND (err u102))
(define-constant ERR-SERVICE-NOT-FOUND (err u103))
(define-constant ERR-INSUFFICIENT-FUNDS (err u104))
(define-constant ERR-ALREADY-MEMBER (err u105))
(define-constant ERR-NOT-MEMBER (err u106))
(define-constant ERR-POOL-INACTIVE (err u107))
(define-constant ERR-INVALID-PERCENTAGE (err u108))

;; Data Variables
(define-data-var next-pool-id uint u1)
(define-data-var next-service-id uint u1)
(define-data-var platform-fee-percentage uint u250) ;; 2.5% in basis points

;; Service Registry
(define-map services
  { service-id: uint }
  {
    name: (string-ascii 50),
    category: (string-ascii 30),
    monthly-cost: uint,
    provider-address: principal,
    active: bool,
    integration-url: (optional (string-ascii 200))
  }
)

;; Pool Registry
(define-map pools
  { pool-id: uint }
  {
    name: (string-ascii 50),
    creator: principal,
    total-monthly-cost: uint,
    current-balance: uint,
    member-count: uint,
    active: bool,
    auto-renewal: bool,
    created-at: uint
  }
)

;; Pool Services - tracks which services are in each pool
(define-map pool-services
  { pool-id: uint, service-id: uint }
  {
    allocation-percentage: uint, ;; basis points (10000 = 100%)
    priority: uint
  }
)

;; Pool Members
(define-map pool-members
  { pool-id: uint, member: principal }
  {
    contribution-percentage: uint, ;; basis points
    total-contributed: uint,
    last-payment: uint,
    active: bool,
    joined-at: uint
  }
)

;; Member Service Preferences
(define-map member-preferences
  { pool-id: uint, member: principal, service-id: uint }
  {
    usage-weight: uint, ;; basis points - how much they want to use this service
    priority: uint
  }
)

;; Payment History
(define-map payment-history
  { pool-id: uint, payment-id: uint }
  {
    amount: uint,
    service-distributions: (list 20 { service-id: uint, amount: uint }),
    timestamp: uint,
    processed-by: principal
  }
)

;; Pool payment counters
(define-map pool-payment-counters
  { pool-id: uint }
  { count: uint }
)

;; Read-only functions

(define-read-only (get-service (service-id uint))
  (map-get? services { service-id: service-id })
)

(define-read-only (get-pool (pool-id uint))
  (map-get? pools { pool-id: pool-id })
)

(define-read-only (get-pool-service (pool-id uint) (service-id uint))
  (map-get? pool-services { pool-id: pool-id, service-id: service-id })
)

(define-read-only (get-pool-member (pool-id uint) (member principal))
  (map-get? pool-members { pool-id: pool-id, member: member })
)

(define-read-only (get-member-preference (pool-id uint) (member principal) (service-id uint))
  (map-get? member-preferences { pool-id: pool-id, member: member, service-id: service-id })
)

(define-read-only (calculate-member-monthly-cost (pool-id uint) (member principal))
  (let (
    (pool-data (unwrap! (get-pool pool-id) (err ERR-POOL-NOT-FOUND)))
    (member-data (unwrap! (get-pool-member pool-id member) (err ERR-NOT-MEMBER)))
  )
    (ok (/ (* (get total-monthly-cost pool-data) (get contribution-percentage member-data)) u10000))
  )
)

(define-read-only (get-pool-services-list (pool-id uint))
  ;; This would need to be implemented with a helper function to iterate through services
  ;; For now, returns ok as placeholder
  (ok true)
)

;; Administrative functions

(define-public (register-service (name (string-ascii 50)) (category (string-ascii 30)) 
                                (monthly-cost uint) (provider-address principal) 
                                (integration-url (optional (string-ascii 200))))
  (let (
    (service-id (var-get next-service-id))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (> monthly-cost u0) ERR-INVALID-AMOUNT)
    
    (map-set services
      { service-id: service-id }
      {
        name: name,
        category: category,
        monthly-cost: monthly-cost,
        provider-address: provider-address,
        active: true,
        integration-url: integration-url
      }
    )
    
    (var-set next-service-id (+ service-id u1))
    (ok service-id)
  )
)

(define-public (update-service-status (service-id uint) (active bool))
  (let (
    (service (unwrap! (get-service service-id) ERR-SERVICE-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    
    (map-set services
      { service-id: service-id }
      (merge service { active: active })
    )
    (ok true)
  )
)

;; Pool Management Functions

(define-public (create-pool (name (string-ascii 50)) (auto-renewal bool))
  (let (
    (pool-id (var-get next-pool-id))
  )
    (map-set pools
      { pool-id: pool-id }
      {
        name: name,
        creator: tx-sender,
        total-monthly-cost: u0,
        current-balance: u0,
        member-count: u0,
        active: true,
        auto-renewal: auto-renewal,
        created-at: stacks-block-height
      }
    )
    
    ;; Add creator as first member with 100% contribution initially
    (map-set pool-members
      { pool-id: pool-id, member: tx-sender }
      {
        contribution-percentage: u10000,
        total-contributed: u0,
        last-payment: u0,
        active: true,
        joined-at: stacks-block-height
      }
    )
    
    (map-set pools
      { pool-id: pool-id }
      (merge (unwrap-panic (get-pool pool-id)) { member-count: u1 })
    )
    
    (var-set next-pool-id (+ pool-id u1))
    (ok pool-id)
  )
)

(define-public (add-service-to-pool (pool-id uint) (service-id uint) (allocation-percentage uint) (priority uint))
  (let (
    (pool-data (unwrap! (get-pool pool-id) ERR-POOL-NOT-FOUND))
    (service-data (unwrap! (get-service service-id) ERR-SERVICE-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get creator pool-data)) ERR-NOT-AUTHORIZED)
    (asserts! (get active pool-data) ERR-POOL-INACTIVE)
    (asserts! (get active service-data) ERR-SERVICE-NOT-FOUND)
    (asserts! (<= allocation-percentage u10000) ERR-INVALID-PERCENTAGE)
    
    (map-set pool-services
      { pool-id: pool-id, service-id: service-id }
      {
        allocation-percentage: allocation-percentage,
        priority: priority
      }
    )
    
    ;; Update total monthly cost
    (let (
      (service-cost (/ (* (get monthly-cost service-data) allocation-percentage) u10000))
      (new-total-cost (+ (get total-monthly-cost pool-data) service-cost))
    )
      (map-set pools
        { pool-id: pool-id }
        (merge pool-data { total-monthly-cost: new-total-cost })
      )
    )
    
    (ok true)
  )
)

(define-public (join-pool (pool-id uint) (contribution-percentage uint))
  (let (
    (pool-data (unwrap! (get-pool pool-id) ERR-POOL-NOT-FOUND))
    (existing-member (get-pool-member pool-id tx-sender))
  )
    (asserts! (get active pool-data) ERR-POOL-INACTIVE)
    (asserts! (is-none existing-member) ERR-ALREADY-MEMBER)
    (asserts! (> contribution-percentage u0) ERR-INVALID-PERCENTAGE)
    (asserts! (<= contribution-percentage u10000) ERR-INVALID-PERCENTAGE)
    
    (map-set pool-members
      { pool-id: pool-id, member: tx-sender }
      {
        contribution-percentage: contribution-percentage,
        total-contributed: u0,
        last-payment: u0,
        active: true,
        joined-at: stacks-block-height
      }
    )
    
    ;; Update member count
    (map-set pools
      { pool-id: pool-id }
      (merge pool-data { member-count: (+ (get member-count pool-data) u1) })
    )
    
    (ok true)
  )
)

(define-public (set-member-service-preference (pool-id uint) (service-id uint) 
                                            (usage-weight uint) (priority uint))
  (let (
    (pool-data (unwrap! (get-pool pool-id) ERR-POOL-NOT-FOUND))
    (member-data (unwrap! (get-pool-member pool-id tx-sender) ERR-NOT-MEMBER))
    (pool-service (unwrap! (get-pool-service pool-id service-id) ERR-SERVICE-NOT-FOUND))
  )
    (asserts! (get active pool-data) ERR-POOL-INACTIVE)
    (asserts! (get active member-data) ERR-NOT-MEMBER)
    (asserts! (<= usage-weight u10000) ERR-INVALID-PERCENTAGE)
    
    (map-set member-preferences
      { pool-id: pool-id, member: tx-sender, service-id: service-id }
      {
        usage-weight: usage-weight,
        priority: priority
      }
    )
    
    (ok true)
  )
)

(define-public (contribute-to-pool (pool-id uint))
  (let (
    (pool-data (unwrap! (get-pool pool-id) ERR-POOL-NOT-FOUND))
    (member-data (unwrap! (get-pool-member pool-id tx-sender) ERR-NOT-MEMBER))
    (monthly-cost (unwrap! (calculate-member-monthly-cost pool-id tx-sender) ERR-INVALID-AMOUNT))
  )
    (asserts! (get active pool-data) ERR-POOL-INACTIVE)
    (asserts! (get active member-data) ERR-NOT-MEMBER)
    
    ;; Transfer STX to contract
    (try! (stx-transfer? monthly-cost tx-sender (as-contract tx-sender)))
    
    ;; Update pool balance
    (map-set pools
      { pool-id: pool-id }
      (merge pool-data { current-balance: (+ (get current-balance pool-data) monthly-cost) })
    )
    
    ;; Update member contribution
    (map-set pool-members
      { pool-id: pool-id, member: tx-sender }
      (merge member-data {
        total-contributed: (+ (get total-contributed member-data) monthly-cost),
        last-payment: stacks-block-height
      })
    )
    
    (ok true)
  )
)

(define-public (process-pool-payments (pool-id uint) (service-distributions (list 20 { service-id: uint, amount: uint })))
  (let (
    (pool-data (unwrap! (get-pool pool-id) ERR-POOL-NOT-FOUND))
    (total-amount (fold + (map get-amount service-distributions) u0))
    (platform-fee (/ (* total-amount (var-get platform-fee-percentage)) u10000))
    (payment-counter (default-to { count: u0 } (map-get? pool-payment-counters { pool-id: pool-id })))
  )
    (asserts! (is-eq tx-sender (get creator pool-data)) ERR-NOT-AUTHORIZED)
    (asserts! (>= (get current-balance pool-data) total-amount) ERR-INSUFFICIENT-FUNDS)
    
    ;; Process payments to service providers
    (try! (fold process-service-payment service-distributions (ok u0)))
    
    ;; Pay platform fee
    (try! (as-contract (stx-transfer? platform-fee tx-sender CONTRACT-OWNER)))
    
    ;; Update pool balance
    (map-set pools
      { pool-id: pool-id }
      (merge pool-data { current-balance: (- (get current-balance pool-data) (+ total-amount platform-fee)) })
    )
    
    ;; Record payment history
    (map-set payment-history
      { pool-id: pool-id, payment-id: (get count payment-counter) }
      {
        amount: total-amount,
        service-distributions: service-distributions,
        timestamp: stacks-block-height,
        processed-by: tx-sender
      }
    )
    
    ;; Update payment counter
    (map-set pool-payment-counters
      { pool-id: pool-id }
      { count: (+ (get count payment-counter) u1) }
    )
    
    (ok true)
  )
)

;; Helper functions

(define-private (get-amount (distribution { service-id: uint, amount: uint }))
  (get amount distribution)
)

(define-private (process-service-payment (distribution { service-id: uint, amount: uint }) (prev-result (response uint uint)))
  (let (
    (service-data (unwrap! (get-service (get service-id distribution)) ERR-SERVICE-NOT-FOUND))
  )
    (if (get active service-data)
      (begin
        (try! (as-contract (stx-transfer? (get amount distribution) tx-sender (get provider-address service-data))))
        (ok u0)
      )
      ERR-SERVICE-NOT-FOUND
    )
  )
)

(define-public (leave-pool (pool-id uint))
  (let (
    (pool-data (unwrap! (get-pool pool-id) ERR-POOL-NOT-FOUND))
    (member-data (unwrap! (get-pool-member pool-id tx-sender) ERR-NOT-MEMBER))
  )
    (asserts! (get active member-data) ERR-NOT-MEMBER)
    
    ;; Deactivate member
    (map-set pool-members
      { pool-id: pool-id, member: tx-sender }
      (merge member-data { active: false })
    )
    
    ;; Update member count
    (map-set pools
      { pool-id: pool-id }
      (merge pool-data { member-count: (- (get member-count pool-data) u1) })
    )
    
    (ok true)
  )
)

(define-public (update-platform-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (<= new-fee u1000) ERR-INVALID-PERCENTAGE) ;; Max 10%
    (var-set platform-fee-percentage new-fee)
    (ok true)
  )
)