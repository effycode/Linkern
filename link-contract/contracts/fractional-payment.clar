;; Subscription Pool Smart Contract
;; Allows users to create and participate in subscription pools with shared costs

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-NOT-MEMBER (err u101))
(define-constant ERR-POOL-NOT-FOUND (err u102))
(define-constant ERR-ALREADY-MEMBER (err u103))
(define-constant ERR-INSUFFICIENT-PAYMENT (err u104))
(define-constant ERR-POOL-FULL (err u105))
(define-constant ERR-INVALID-CYCLE (err u106))

;; Data maps
(define-map subscription-pools
  { pool-id: uint }
  {
    total-fee: uint,
    cycle-start: uint,
    cycle-length: uint,
    admin: principal,
    member-count: uint,
    max-members: uint,
    service-name: (string-ascii 50)
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

;; Data variables
(define-data-var next-pool-id uint u1)

;; Create a new subscription pool
(define-public (create-pool (total-fee uint) (cycle-length uint) (max-members uint) (service-name (string-ascii 50)))
  (let
    (
      (pool-id (var-get next-pool-id))
      (caller tx-sender)
    )
    ;; Create the pool
    (map-set subscription-pools
      { pool-id: pool-id }
      {
        total-fee: total-fee,
        cycle-start: stacks-block-height,
        cycle-length: cycle-length,
        admin: caller,
        member-count: u0,
        max-members: max-members,
        service-name: service-name
      }
    )
    
    ;; Increment pool ID counter
    (var-set next-pool-id (+ pool-id u1))
    (ok pool-id)
  )
)

;; Join a subscription pool
(define-public (join-pool (pool-id uint))
  (let
    (
      (caller tx-sender)
      (pool (unwrap! (map-get? subscription-pools { pool-id: pool-id }) ERR-POOL-NOT-FOUND))
      (current-members (get member-count pool))
      (max-members (get max-members pool))
    )
    ;; Check if already a member
    (asserts! (is-none (map-get? pool-members { pool-id: pool-id, member: caller })) ERR-ALREADY-MEMBER)
    
    ;; Check if pool has space
    (asserts! (< current-members max-members) ERR-POOL-FULL)
    
    ;; Remove try! from map-set operations since they return bool, not response
    ;; Add member to pool
    (map-set pool-members
      { pool-id: pool-id, member: caller }
      {
        joined-at: stacks-block-height,
        active: true,
        contribution: u0
      }
    )
    
    ;; Update pool member count
    (map-set subscription-pools
      { pool-id: pool-id }
      {
        total-fee: (get total-fee pool),
        cycle-start: (get cycle-start pool),
        cycle-length: (get cycle-length pool),
        admin: (get admin pool),
        member-count: (+ current-members u1),
        max-members: (get max-members pool),
        service-name: (get service-name pool)
      }
    )
    
    ;; Only use try! on adjust-contributions since it returns a response
    (try! (adjust-contributions pool-id))
    
    ;; Return success
    (ok true)
  )
)

;; Leave a subscription pool
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
        member-count: (- (get member-count pool) u1),
        max-members: (get max-members pool),
        service-name: (get service-name pool)
      }
    )
    
    ;; Mark member as inactive
    (map-set pool-members
      { pool-id: pool-id, member: caller }
      {
        joined-at: (get joined-at member-data),
        active: false,
        contribution: u0
      }
    )
    
    ;; Recalculate shares for remaining members
    (try! (adjust-contributions pool-id))
    
    ;; Return single success response
    (ok true)
  )
)

;; Adjust contributions for all active members in a pool
(define-private (adjust-contributions (pool-id uint))
  (let
    (
      (pool (unwrap! (map-get? subscription-pools { pool-id: pool-id }) ERR-POOL-NOT-FOUND))
      (active-members (get member-count pool))
      (total-fee (get total-fee pool))
    )
    ;; Calculate per-member contribution
    (let ((per-member-fee (/ total-fee active-members)))
      ;; This is a simplified implementation - in a real contract you'd iterate through members
      ;; For now, just return success
      (ok true)
    )
  )
)


;; Pay contribution for current cycle
(define-public (pay-contribution (pool-id uint))
  (let
    (
      (caller tx-sender)
      (pool (unwrap! (map-get? subscription-pools { pool-id: pool-id }) ERR-POOL-NOT-FOUND))
      (member-data (unwrap! (map-get? pool-members { pool-id: pool-id, member: caller }) ERR-NOT-MEMBER))
      (contribution-amount (calculate-contribution pool-id))
    )
    ;; Check if member is active
    (asserts! (get active member-data) ERR-NOT-MEMBER)
    
    ;; Transfer STX from member to contract
    (try! (stx-transfer? contribution-amount caller (as-contract tx-sender)))
    
    ;; Update member contribution
    (map-set pool-members
      { pool-id: pool-id, member: caller }
      {
        joined-at: (get joined-at member-data),
        active: (get active member-data),
        contribution: contribution-amount
      }
    )
    
    (ok contribution-amount)
  )
)


;; Calculate individual contribution based on current active members
(define-read-only (calculate-contribution (pool-id uint))
  (let
    (
      (pool (unwrap! (map-get? subscription-pools { pool-id: pool-id }) u0))
      (member-count (get member-count pool))
      (total-fee (get total-fee pool))
    )
    (if (> member-count u0)
      (/ total-fee member-count)
      u0
    )
  )
)

;; Get pool information
(define-read-only (get-pool-info (pool-id uint))
  (map-get? subscription-pools { pool-id: pool-id })
)

;; Get member information
(define-read-only (get-member-info (pool-id uint) (member principal))
  (map-get? pool-members { pool-id: pool-id, member: member })
)

;; Check if user is active member of pool
(define-read-only (is-active-member (pool-id uint) (member principal))
  (match (map-get? pool-members { pool-id: pool-id, member: member })
    member-data (get active member-data)
    false
  )
)

;; Get current cycle information
(define-read-only (get-cycle-info (pool-id uint))
  (let
    (
      (pool (unwrap! (map-get? subscription-pools { pool-id: pool-id }) (err ERR-POOL-NOT-FOUND)))
      (cycle-start (get cycle-start pool))
      (cycle-length (get cycle-length pool))
      (current-block stacks-block-height)
    )
    (ok {
      cycle-start: cycle-start,
      cycle-length: cycle-length,
      current-cycle: (/ (- current-block cycle-start) cycle-length),
      blocks-until-next: (- cycle-length (mod (- current-block cycle-start) cycle-length))
    })
  )
)
