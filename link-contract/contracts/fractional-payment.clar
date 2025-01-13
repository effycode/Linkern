;; subscription-pool
;; Manages shared subscription payments with auto-adjusting contributions

(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ALREADY-MEMBER (err u101))
(define-constant ERR-NOT-MEMBER (err u102))
(define-constant ERR-INSUFFICIENT-BALANCE (err u103))

;; Data Maps
(define-map subscription-pools
  { pool-id: uint }
  {
    total-fee: uint,
    cycle-start: uint,
    cycle-length: uint,
    admin: principal,
    member-count: uint  ;; Added to track total members
  }
)

(define-map pool-members
  { pool-id: uint, member: principal }
  {
    joined-at: uint,
    active: bool,
    contribution: uint
  }
)

(define-map pool-balances
  { pool-id: uint }
  { current-balance: uint }
)

;; Read-only functions
(define-read-only (get-member-contribution (pool-id uint) (member principal))
  (match (map-get? pool-members { pool-id: pool-id, member: member })
    member-data (ok member-data)
    (err ERR-NOT-MEMBER)
  )
)

;; Fixed function to get active member count
(define-read-only (get-active-member-count (pool-id uint))
  (get member-count (default-to 
    { total-fee: u0, cycle-start: u0, cycle-length: u0, admin: tx-sender, member-count: u0 }
    (map-get? subscription-pools { pool-id: pool-id })))
)

(define-read-only (calculate-share (pool-id uint))
  (let
    (
      (pool (unwrap! (map-get? subscription-pools { pool-id: pool-id }) ERR-NOT-AUTHORIZED))
      (active-members (get member-count pool))
    )
    (if (> active-members u0)
      (ok (/ (get total-fee pool) active-members))
      (ok u0)
    )
  )
)

;; Public functions
(define-public (create-pool (pool-id uint) (total-fee uint) (cycle-length uint))
  (let
    (
      (caller tx-sender)
    )
    (map-set subscription-pools
      { pool-id: pool-id }
      {
        total-fee: total-fee,
        cycle-start: stacks-block-height,
        cycle-length: cycle-length,
        admin: caller,
        member-count: u0  ;; Initialize member count
      }
    )
    (map-set pool-balances
      { pool-id: pool-id }
      { current-balance: u0 }
    )
    (ok true)
  )
)

(define-public (join-pool (pool-id uint))
  (let
    (
      (caller tx-sender)
      (pool (unwrap! (map-get? subscription-pools { pool-id: pool-id }) ERR-NOT-AUTHORIZED))
      (current-share (unwrap! (calculate-share pool-id) ERR-NOT-AUTHORIZED))
    )
    (asserts! (is-none (map-get? pool-members { pool-id: pool-id, member: caller })) ERR-ALREADY-MEMBER)
    
    ;; Update pool member count first
    (map-set subscription-pools
      { pool-id: pool-id }
      {
        total-fee: (get total-fee pool),
        cycle-start: (get cycle-start pool),
        cycle-length: (get cycle-length pool),
        admin: (get admin pool),
        member-count: (+ (get member-count pool) u1)
      })
    
    ;; Then add member
    (map-set pool-members
      { pool-id: pool-id, member: caller }
      {
        joined-at: stacks-block-height,
        active: true,
        contribution: current-share
      })
    
    ;; Finally recalculate shares
    (try! (adjust-contributions pool-id))
    (ok true)
  )
)

(define-public (leave-pool (pool-id uint))
  (let
    (
      (caller tx-sender)
      (pool (unwrap! (map-get? subscription-pools { pool-id: pool-id }) ERR-NOT-AUTHORIZED))
      (member-data (unwrap! (map-get? pool-members { pool-id: pool-id, member: caller }) ERR-NOT-MEMBER))
    )
    ;; Update pool member count
    (map-set subscription-pools
      { pool-id: pool-id }
      {
        total-fee: (get total-fee pool),
        cycle-start: (get cycle-start pool),
        cycle-length: (get cycle-length pool),
        admin: (get admin pool),
        member-count: (- (get member-count pool) u1)
      })
    
    ;; Mark member as inactive
    (map-set pool-members
      { pool-id: pool-id, member: caller }
      {
        joined-at: (get joined-at member-data),
        active: false,
        contribution: u0
      })
    
    ;; Recalculate shares for remaining members - need to check response
    (try! (adjust-contributions pool-id))
    (ok true)
  )
)

(define-private (adjust-contributions (pool-id uint))
  (let
    (
      (new-share (unwrap! (calculate-share pool-id) ERR-NOT-AUTHORIZED))
      (pool (unwrap! (map-get? subscription-pools { pool-id: pool-id }) ERR-NOT-AUTHORIZED))
    )
    ;; Update contribution amount for all active members
    (map-set pool-members
      { pool-id: pool-id, member: tx-sender }
      {
        joined-at: stacks-block-height,
        active: true,
        contribution: new-share
      }
    )
    (ok true)
  )
)

(define-public (contribute (pool-id uint) (amount uint))
  (let
    (
      (caller tx-sender)
      (member-data (unwrap! (map-get? pool-members { pool-id: pool-id, member: caller }) ERR-NOT-MEMBER))
      (pool-balance (unwrap! (map-get? pool-balances { pool-id: pool-id }) ERR-NOT-AUTHORIZED))
    )
    
    ;; Transfer tokens from member to contract
    (try! (stx-transfer? amount caller (as-contract tx-sender)))
    
    ;; Update pool balance
    (map-set pool-balances
      { pool-id: pool-id }
      { current-balance: (+ (get current-balance pool-balance) amount) }
    )
    (ok true)
  )
)

;; Administrative functions
(define-public (process-payment (pool-id uint) (recipient principal))
  (let
    (
      (caller tx-sender)
      (pool (unwrap! (map-get? subscription-pools { pool-id: pool-id }) ERR-NOT-AUTHORIZED))
      (pool-balance (unwrap! (map-get? pool-balances { pool-id: pool-id }) ERR-NOT-AUTHORIZED))
    )
    ;; Verify caller is admin
    (asserts! (is-eq caller (get admin pool)) ERR-NOT-AUTHORIZED)
    ;; Verify sufficient balance
    (asserts! (>= (get current-balance pool-balance) (get total-fee pool)) ERR-INSUFFICIENT-BALANCE)
    
    ;; Transfer payment to recipient
    (try! (as-contract (stx-transfer? (get total-fee pool) tx-sender recipient)))
    
    ;; Reset pool balance
    (map-set pool-balances
      { pool-id: pool-id }
      { current-balance: (- (get current-balance pool-balance) (get total-fee pool)) }
    )
    (ok true)
  )
)