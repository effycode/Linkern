;; NFT-Based Subscription Pool Membership Contract
;; This contract manages NFT membership tokens for shared subscription access

;; Define the NFT
(define-non-fungible-token subscription-membership uint)

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-token-owner (err u101))
(define-constant err-token-not-found (err u102))
(define-constant err-insufficient-payment (err u103))
(define-constant err-pool-full (err u104))
(define-constant err-subscription-expired (err u105))
(define-constant err-invalid-service (err u106))

;; Data Variables
(define-data-var token-id-nonce uint u0)
(define-data-var pool-price uint u1000000) ;; Price in microSTX
(define-data-var max-pool-size uint u10)

;; Data Maps
(define-map token-metadata 
  uint 
  {
    service-type: (string-ascii 50),
    expiry-block: uint,
    usage-rights: (string-ascii 100),
    pool-id: uint,
    original-owner: principal
  }
)

(define-map pool-info
  uint
  {
    service-name: (string-ascii 50),
    monthly-cost: uint,
    current-members: uint,
    subscription-blocks: uint, ;; Duration in blocks
    pool-creator: principal,
    is-active: bool
  }
)

(define-map user-access
  {user: principal, service-type: (string-ascii 50)}
  {
    token-id: uint,
    last-access: uint,
    access-count: uint
  }
)

(define-map pool-members
  {pool-id: uint, member: principal}
  {
    token-id: uint,
    joined-block: uint,
    contribution: uint
  }
)

;; Pool ID counter
(define-data-var pool-id-nonce uint u0)

;; Read-only functions

;; Get token metadata
(define-read-only (get-token-metadata (token-id uint))
  (map-get? token-metadata token-id)
)

;; Get pool information
(define-read-only (get-pool-info (pool-id uint))
  (map-get? pool-info pool-id)
)

;; Check if user has access to a service
(define-read-only (has-access (user principal) (service (string-ascii 50)))
  (match (map-get? user-access {user: user, service-type: service})
    access-info 
    (let ((token-id (get token-id access-info)))
      (match (map-get? token-metadata token-id)
        metadata
        (and 
          (is-eq (nft-get-owner? subscription-membership token-id) (some user))
          (> (get expiry-block metadata) stacks-block-height)
        )
        false
      )
    )
    false
  )
)

;; Get user's token for a specific service
(define-read-only (get-user-token (user principal) (service (string-ascii 50)))
  (map-get? user-access {user: user, service-type: service})
)

;; Check token ownership
(define-read-only (get-owner (token-id uint))
  (ok (nft-get-owner? subscription-membership token-id))
)

;; Get last token ID
(define-read-only (get-last-token-id)
  (ok (var-get token-id-nonce))
)

;; Public functions

;; Create a new subscription pool
(define-public (create-pool 
  (service-name (string-ascii 50))
  (monthly-cost uint)
  (subscription-blocks uint)
)
  (let 
    (
      (new-pool-id (+ (var-get pool-id-nonce) u1))
    )
    (map-set pool-info new-pool-id {
      service-name: service-name,
      monthly-cost: monthly-cost,
      current-members: u0,
      subscription-blocks: subscription-blocks,
      pool-creator: tx-sender,
      is-active: true
    })
    (var-set pool-id-nonce new-pool-id)
    (ok new-pool-id)
  )
)

;; Join a subscription pool and mint NFT
(define-public (join-pool (pool-id uint))
  (let 
    (
      (pool (unwrap! (map-get? pool-info pool-id) err-token-not-found))
      (current-members (get current-members pool))
      (max-members (var-get max-pool-size))
      (cost-per-member (/ (get monthly-cost pool) max-members))
      (new-token-id (+ (var-get token-id-nonce) u1))
      (expiry-block (+ stacks-block-height (get subscription-blocks pool)))
    )
    
    ;; Check if pool has space
    (asserts! (< current-members max-members) err-pool-full)
    
    ;; Check if pool is active
    (asserts! (get is-active pool) err-token-not-found)
    
    ;; Transfer payment
    (try! (stx-transfer? cost-per-member tx-sender (get pool-creator pool)))
    
    ;; Mint NFT
    (try! (nft-mint? subscription-membership new-token-id tx-sender))
    
    ;; Set token metadata
    (map-set token-metadata new-token-id {
      service-type: (get service-name pool),
      expiry-block: expiry-block,
      usage-rights: "full-access",
      pool-id: pool-id,
      original-owner: tx-sender
    })
    
    ;; Update user access
    (map-set user-access 
      {user: tx-sender, service-type: (get service-name pool)}
      {
        token-id: new-token-id,
        last-access: stacks-block-height,
        access-count: u0
      }
    )
    
    ;; Add to pool members
    (map-set pool-members
      {pool-id: pool-id, member: tx-sender}
      {
        token-id: new-token-id,
        joined-block: stacks-block-height,
        contribution: cost-per-member
      }
    )
    
    ;; Update pool info
    (map-set pool-info pool-id 
      (merge pool {current-members: (+ current-members u1)})
    )
    
    ;; Update token ID nonce
    (var-set token-id-nonce new-token-id)
    
    (ok new-token-id)
  )
)

;; Transfer NFT (enables secondary market)
(define-public (transfer (token-id uint) (sender principal) (recipient principal))
  (begin
    ;; Check ownership
    (asserts! (is-eq tx-sender sender) err-not-token-owner)
    (asserts! (is-eq (some sender) (nft-get-owner? subscription-membership token-id)) err-not-token-owner)
    
    ;; Transfer NFT
    (try! (nft-transfer? subscription-membership token-id sender recipient))
    
    ;; Update user access for recipient
    (match (map-get? token-metadata token-id)
      metadata
      (map-set user-access 
        {user: recipient, service-type: (get service-type metadata)}
        {
          token-id: token-id,
          last-access: stacks-block-height,
          access-count: u0
        }
      )
      false
    )
    
    ;; Remove access for sender
    (match (map-get? token-metadata token-id)
      metadata
      (map-delete user-access {user: sender, service-type: (get service-type metadata)})
      false
    )
    
    (ok true)
  )
)

;; Verify access and log usage
(define-public (verify-and-log-access (service (string-ascii 50)))
  (let 
    (
      (access-info (unwrap! (map-get? user-access {user: tx-sender, service-type: service}) err-token-not-found))
      (token-id (get token-id access-info))
      (metadata (unwrap! (map-get? token-metadata token-id) err-token-not-found))
    )
    
    ;; Check if subscription is still valid
    (asserts! (> (get expiry-block metadata) stacks-block-height) err-subscription-expired)
    
    ;; Check NFT ownership
    (asserts! (is-eq (nft-get-owner? subscription-membership token-id) (some tx-sender)) err-not-token-owner)
    
    ;; Update access log
    (map-set user-access 
      {user: tx-sender, service-type: service}
      {
        token-id: token-id,
        last-access: stacks-block-height,
        access-count: (+ (get access-count access-info) u1)
      }
    )
    
    (ok true)
  )
)

;; Extend subscription (pool creator only)
(define-public (extend-subscription (pool-id uint) (additional-blocks uint))
  (let 
    (
      (pool (unwrap! (map-get? pool-info pool-id) err-token-not-found))
    )
    
    ;; Check if caller is pool creator
    (asserts! (is-eq tx-sender (get pool-creator pool)) err-owner-only)
    
    ;; Update pool info
    (map-set pool-info pool-id 
      (merge pool {subscription-blocks: (+ (get subscription-blocks pool) additional-blocks)})
    )
    
    (ok true)
  )
)

;; Deactivate pool (pool creator only)
(define-public (deactivate-pool (pool-id uint))
  (let 
    (
      (pool (unwrap! (map-get? pool-info pool-id) err-token-not-found))
    )
    
    ;; Check if caller is pool creator
    (asserts! (is-eq tx-sender (get pool-creator pool)) err-owner-only)
    
    ;; Deactivate pool
    (map-set pool-info pool-id 
      (merge pool {is-active: false})
    )
    
    (ok true)
  )
)

;; Update pool settings (contract owner only)
(define-public (update-pool-settings (new-price uint) (new-max-size uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set pool-price new-price)
    (var-set max-pool-size new-max-size)
    (ok true)
  )
)

;; Get token URI (for NFT metadata)
(define-read-only (get-token-uri (token-id uint))
  (ok (some (concat "https://api.subscriptionpool.com/metadata/" (uint-to-ascii token-id))))
)

;; Helper function to convert uint to ascii (simplified)
(define-read-only (uint-to-ascii (value uint))
  (if (is-eq value u0) "0"
  (if (is-eq value u1) "1"
  (if (is-eq value u2) "2"
  (if (is-eq value u3) "3"
  (if (is-eq value u4) "4"
  (if (is-eq value u5) "5"
  (if (is-eq value u6) "6"
  (if (is-eq value u7) "7"
  (if (is-eq value u8) "8"
  (if (is-eq value u9) "9"
  "unknown"))))))))))
)
