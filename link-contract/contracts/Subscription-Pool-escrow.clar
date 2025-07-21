;; Subscription Pool Escrow Smart Contract
;; Handles automated payments, escrow, and refunds for subscription pools

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-pool-not-active (err u104))
(define-constant err-already-paid (err u105))
(define-constant err-pool-not-funded (err u106))
(define-constant err-already-activated (err u107))
(define-constant err-invalid-participant (err u108))
(define-constant err-pool-expired (err u109))

;; Data Variables
(define-data-var next-pool-id uint u1)
(define-data-var platform-fee-percent uint u250) ;; 2.5% = 250 basis points

;; Pool status enum
(define-constant pool-status-active u1)
(define-constant pool-status-funded u2)
(define-constant pool-status-activated u3)
(define-constant pool-status-cancelled u4)

;; Data Maps
(define-map pools
  { pool-id: uint }
  {
    creator: principal,
    subscription-provider: principal,
    total-cost: uint,
    cost-per-participant: uint,
    max-participants: uint,
    current-participants: uint,
    status: uint,
    created-at: uint,
    duration-blocks: uint,
    service-start-block: uint,
    escrow-balance: uint
  }
)

(define-map pool-participants
  { pool-id: uint, participant: principal }
  {
    amount-paid: uint,
    joined-at: uint,
    is-active: bool
  }
)

(define-map participant-pools
  { participant: principal, pool-id: uint }
  { is-member: bool }
)

;; Read-only functions
(define-read-only (get-pool (pool-id uint))
  (map-get? pools { pool-id: pool-id })
)

(define-read-only (get-participant-info (pool-id uint) (participant principal))
  (map-get? pool-participants { pool-id: pool-id, participant: participant })
)

(define-read-only (is-pool-member (pool-id uint) (participant principal))
  (match (map-get? participant-pools { participant: participant, pool-id: pool-id })
    entry (get is-member entry)
    false
  )
)

(define-read-only (calculate-refund (pool-id uint) (participant principal))
  (let (
    (pool-info (unwrap! (get-pool pool-id) (err err-not-found)))
    (participant-info (unwrap! (get-participant-info pool-id participant) (err err-invalid-participant)))
    (blocks-elapsed (- stacks-block-height (get service-start-block pool-info)))
    (total-duration (get duration-blocks pool-info))
    (amount-paid (get amount-paid participant-info))
  )
    (if (>= blocks-elapsed total-duration)
      (ok u0) ;; No refund if service period is over
      (let (
        (remaining-blocks (- total-duration blocks-elapsed))
        (refund-amount (/ (* amount-paid remaining-blocks) total-duration))
      )
        (ok refund-amount)
      )
    )
  )
)

(define-read-only (get-pool-funding-status (pool-id uint))
  (match (get-pool pool-id)
    pool-info
    (let (
      (required-amount (get total-cost pool-info))
      (current-balance (get escrow-balance pool-info))
    )
      (ok {
        required: required-amount,
        current: current-balance,
        is-fully-funded: (>= current-balance required-amount)
      })
    )
    (err err-not-found)
  )
)

;; Private functions
(define-private (add-to-escrow (pool-id uint) (amount uint))
  (let (
    (pool-info (unwrap! (get-pool pool-id) (err err-not-found)))
    (new-balance (+ (get escrow-balance pool-info) amount))
  )
    (map-set pools
      { pool-id: pool-id }
      (merge pool-info { escrow-balance: new-balance })
    )
    (ok new-balance)
  )
)

(define-private (deduct-from-escrow (pool-id uint) (amount uint))
  (let (
    (pool-info (unwrap! (get-pool pool-id) (err err-not-found)))
    (current-balance (get escrow-balance pool-info))
  )
    (asserts! (>= current-balance amount) (err err-invalid-amount))
    (let (
      (new-balance (- current-balance amount))
    )
      (map-set pools
        { pool-id: pool-id }
        (merge pool-info { escrow-balance: new-balance })
      )
      (ok new-balance)
    )
  )
)

;; Public functions
(define-public (create-pool 
  (subscription-provider principal)
  (total-cost uint)
  (max-participants uint)
  (duration-blocks uint)
)
  (let (
    (pool-id (var-get next-pool-id))
    (cost-per-participant (/ total-cost max-participants))
  )
    (asserts! (> total-cost u0) (err err-invalid-amount))
    (asserts! (> max-participants u0) (err err-invalid-amount))
    (asserts! (> duration-blocks u0) (err err-invalid-amount))
    
    (map-set pools
      { pool-id: pool-id }
      {
        creator: tx-sender,
        subscription-provider: subscription-provider,
        total-cost: total-cost,
        cost-per-participant: cost-per-participant,
        max-participants: max-participants,
        current-participants: u0,
        status: pool-status-active,
        created-at: stacks-block-height,
        duration-blocks: duration-blocks,
        service-start-block: u0,
        escrow-balance: u0
      }
    )
    
    (var-set next-pool-id (+ pool-id u1))
    (ok pool-id)
  )
)

(define-public (join-pool (pool-id uint))
  (let (
    (pool-info (unwrap! (get-pool pool-id) (err err-not-found)))
    (payment-amount (get cost-per-participant pool-info))
  )
    ;; Validate pool state
    (asserts! (is-eq (get status pool-info) pool-status-active) (err err-pool-not-active))
    (asserts! (< (get current-participants pool-info) (get max-participants pool-info)) (err err-pool-not-active))
    (asserts! (not (is-pool-member pool-id tx-sender)) (err err-already-paid))
    
    ;; Transfer tokens to contract
    (unwrap! (stx-transfer? payment-amount tx-sender (as-contract tx-sender)) (err err-invalid-amount))
    
    ;; Add to escrow
    (try! (add-to-escrow pool-id payment-amount))
    
    ;; Update participant info
    (map-set pool-participants
      { pool-id: pool-id, participant: tx-sender }
      {
        amount-paid: payment-amount,
        joined-at: stacks-block-height,
        is-active: true
      }
    )
    
    (map-set participant-pools
      { participant: tx-sender, pool-id: pool-id }
      { is-member: true }
    )
    
    ;; Update pool participant count
    (let (
      (new-participant-count (+ (get current-participants pool-info) u1))
    )
      (map-set pools
        { pool-id: pool-id }
        (merge pool-info { 
          current-participants: new-participant-count,
          status: (if (>= new-participant-count (get max-participants pool-info))
                    pool-status-funded
                    pool-status-active)
        })
      )
      (ok true)
    )
  )
)

(define-public (activate-subscription (pool-id uint))
  (let (
    (pool-info (unwrap! (get-pool pool-id) (err err-not-found)))
  )
    ;; Only pool creator or contract owner can activate
    (asserts! (or (is-eq tx-sender (get creator pool-info)) 
                  (is-eq tx-sender contract-owner)) 
              (err err-unauthorized))
    
    ;; Pool must be fully funded
    (asserts! (is-eq (get status pool-info) pool-status-funded) (err err-pool-not-funded))
    (asserts! (>= (get escrow-balance pool-info) (get total-cost pool-info)) (err err-pool-not-funded))
    
    ;; Calculate platform fee
    (let (
      (total-amount (get total-cost pool-info))
      (platform-fee (/ (* total-amount (var-get platform-fee-percent)) u10000))
      (provider-payment (- total-amount platform-fee))
    )
      ;; Transfer payment to subscription provider
      (unwrap! (as-contract (stx-transfer? provider-payment tx-sender (get subscription-provider pool-info))) (err err-invalid-amount))
      
      ;; Transfer platform fee to contract owner
      (unwrap! (as-contract (stx-transfer? platform-fee tx-sender contract-owner)) (err err-invalid-amount))
      
      ;; Update pool status
      (map-set pools
        { pool-id: pool-id }
        (merge pool-info {
          status: pool-status-activated,
          service-start-block: stacks-block-height,
          escrow-balance: u0
        })
      )
      
      (ok true)
    )
  )
)

(define-public (leave-pool (pool-id uint))
  (let (
    (pool-info (unwrap! (get-pool pool-id) (err err-not-found)))
    (participant-info (unwrap! (get-participant-info pool-id tx-sender) (err err-invalid-participant)))
  )
    ;; Ensure participant is active member
    (asserts! (get is-active participant-info) (err err-invalid-participant))
    (asserts! (is-pool-member pool-id tx-sender) (err err-invalid-participant))
    
    (if (is-eq (get status pool-info) pool-status-activated)
      ;; Pool is activated - calculate and process refund
      (let (
        (refund-amount (unwrap! (calculate-refund pool-id tx-sender) (err err-invalid-amount)))
      )
        (if (> refund-amount u0)
          (unwrap! (as-contract (stx-transfer? refund-amount tx-sender tx-sender)) (err err-invalid-amount))
          true
        )
        
        ;; Update participant status
        (map-set pool-participants
          { pool-id: pool-id, participant: tx-sender }
          (merge participant-info { is-active: false })
        )
        
        (map-set participant-pools
          { participant: tx-sender, pool-id: pool-id }
          { is-member: false }
        )
        
        (ok refund-amount)
      )
      ;; Pool not activated - full refund from escrow
      (let (
        (refund-amount (get amount-paid participant-info))
      )
        (try! (deduct-from-escrow pool-id refund-amount))
        (unwrap! (as-contract (stx-transfer? refund-amount tx-sender tx-sender)) (err err-invalid-amount))
        
        ;; Update participant status
        (map-set pool-participants
          { pool-id: pool-id, participant: tx-sender }
          (merge participant-info { is-active: false })
        )
        
        (map-set participant-pools
          { participant: tx-sender, pool-id: pool-id }
          { is-member: false }
        )
        
        ;; Update pool participant count
        (map-set pools
          { pool-id: pool-id }
          (merge pool-info {
            current-participants: (- (get current-participants pool-info) u1),
            status: pool-status-active
          })
        )
        
        (ok refund-amount)
      )
    )
  )
)

(define-public (cancel-pool (pool-id uint))
  (let (
    (pool-info (unwrap! (get-pool pool-id) (err err-not-found)))
  )
    ;; Only creator or contract owner can cancel
    (asserts! (or (is-eq tx-sender (get creator pool-info)) 
                  (is-eq tx-sender contract-owner)) 
              (err err-unauthorized))
    
    ;; Pool must not be activated
    (asserts! (not (is-eq (get status pool-info) pool-status-activated)) (err err-already-activated))
    
    ;; Update pool status
    (map-set pools
      { pool-id: pool-id }
      (merge pool-info { status: pool-status-cancelled })
    )
    
    (ok true)
  )
)

;; Admin functions
(define-public (set-platform-fee (new-fee-percent uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) (err err-owner-only))
    (asserts! (<= new-fee-percent u1000) (err err-invalid-amount)) ;; Max 10%
    (var-set platform-fee-percent new-fee-percent)
    (ok true)
  )
)

(define-public (emergency-withdraw (pool-id uint))
  (let (
    (pool-info (unwrap! (get-pool pool-id) (err err-not-found)))
  )
    (asserts! (is-eq tx-sender contract-owner) (err err-owner-only))
    (let (
      (balance (get escrow-balance pool-info))
    )
      (try! (deduct-from-escrow pool-id balance))
      (unwrap! (as-contract (stx-transfer? balance tx-sender contract-owner)) (err err-invalid-amount))
      (ok balance)
    )
  )
)