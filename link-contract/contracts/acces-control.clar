;; Smart Contract-Powered Access Control
;; Implements token-activated, time-locked, and usage-based access control

;; Define the payment token
(define-fungible-token access-token)

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED u1)
(define-constant ERR-INSUFFICIENT-FUNDS u2)
(define-constant ERR-INVALID-PERIOD u3)
(define-constant ERR-NO-ACTIVE-SUBSCRIPTION u4)
(define-constant ERR-INSUFFICIENT-USAGE-CREDITS u5)
(define-constant ERR-POOL-UNDERFUNDED u6)

;; Access periods in blocks (approximately)
(define-constant DAILY-PERIOD u144) ;; ~144 blocks per day (10 min block time)
(define-constant WEEKLY-PERIOD u1008) ;; ~1008 blocks per week
(define-constant MONTHLY-PERIOD u4320) ;; ~4320 blocks per month

;; Pricing (in tokens)
(define-data-var daily-price uint u10)
(define-data-var weekly-price uint u60)
(define-data-var monthly-price uint u200)
(define-data-var usage-rate uint u1) ;; Cost per unit of usage

;; Minimum pool funding threshold (percentage of total subscriptions)
(define-data-var min-pool-funding-pct uint u80) ;; 80%

;; Data maps
(define-map time-subscriptions 
  { user: principal } 
  { 
    start-block: uint,
    end-block: uint,
    period-type: (string-utf8 10),
    auto-renew: bool,
    last-payment: uint
  }
)

(define-map usage-subscriptions
  { user: principal }
  {
    credits: uint,
    last-usage-block: uint
  }
)

(define-map pool-contributions
  { user: principal }
  {
    total-contributed: uint,
    current-share: uint
  }
)

(define-map service-access-log
  { user: principal, service-id: (string-utf8 36) }
  {
    last-access: uint,
    total-accesses: uint
  }
)

;; Pool data
(define-data-var total-pool-funds uint u0)
(define-data-var total-required-funds uint u0)

;; Initialize the contract
(define-public (initialize (initial-supply uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) (err ERR-NOT-AUTHORIZED))
    (try! (ft-mint? access-token initial-supply CONTRACT-OWNER))
    (ok true)
  )
)

;; Update pricing
(define-public (update-pricing (new-daily uint) (new-weekly uint) (new-monthly uint) (new-usage-rate uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) (err ERR-NOT-AUTHORIZED))
    (var-set daily-price new-daily)
    (var-set weekly-price new-weekly)
    (var-set monthly-price new-monthly)
    (var-set usage-rate new-usage-rate)
    (ok true)
  )
)

;; Update minimum pool funding threshold
(define-public (update-min-pool-funding (new-threshold uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) (err ERR-NOT-AUTHORIZED))
    (asserts! (<= new-threshold u100) (err u7)) ;; Cannot exceed 100%
    (var-set min-pool-funding-pct new-threshold)
    (ok true)
  )
)

;; ========== TOKEN-ACTIVATED ACCESS FUNCTIONS ==========

;; Contribute to the pool
(define-public (contribute-to-pool (amount uint))
  (let (
    (current-contribution (default-to { total-contributed: u0, current-share: u0 } 
                          (map-get? pool-contributions { user: tx-sender })))
    (new-total-contributed (+ (get total-contributed current-contribution) amount))
    (new-pool-total (+ (var-get total-pool-funds) amount))
  )
    ;; Transfer tokens to contract
    (try! (ft-transfer? access-token amount tx-sender (as-contract tx-sender)))
    
    ;; Update user's contribution record
    (map-set pool-contributions 
      { user: tx-sender } 
      { 
        total-contributed: new-total-contributed, 
        current-share: (+ (get current-share current-contribution) amount)
      }
    )
    
    ;; Update pool total
    (var-set total-pool-funds new-pool-total)
    
    (ok true)
  )
)

;; ========== TIME-LOCKED ACCESS FUNCTIONS ==========


;; Check if a user has time-based access
(define-read-only (has-time-access (user principal))
  (let (
    (subscription (map-get? time-subscriptions { user: user }))
  )
    (and
      (is-some subscription)
      (<= stacks-block-height (get end-block (unwrap! subscription false)))
    )
  )
)


;; ========== USAGE-BASED ACCESS FUNCTIONS ==========

;; Purchase usage credits
(define-public (purchase-usage-credits (amount uint))
  (let (
    (cost (* amount (var-get usage-rate)))
    (current-credits (default-to { credits: u0, last-usage-block: u0 } 
                     (map-get? usage-subscriptions { user: tx-sender })))
  )
    ;; Transfer tokens for payment
    (try! (ft-transfer? access-token cost tx-sender (as-contract tx-sender)))
    
    ;; Update pool funds
    (var-set total-pool-funds (+ (var-get total-pool-funds) cost))
    
    ;; Update user's usage credits
    (map-set usage-subscriptions
      { user: tx-sender }
      {
        credits: (+ (get credits current-credits) amount),
        last-usage-block: stacks-block-height
      }
    )
    
    ;; Update user's pool contribution
    (let (
      (current-contribution (default-to { total-contributed: u0, current-share: u0 } 
                            (map-get? pool-contributions { user: tx-sender })))
    )
      (map-set pool-contributions
        { user: tx-sender }
        {
          total-contributed: (+ (get total-contributed current-contribution) cost),
          current-share: (+ (get current-share current-contribution) cost)
        }
      )
    )
    
    (ok (+ (get credits current-credits) amount))
  )
)

