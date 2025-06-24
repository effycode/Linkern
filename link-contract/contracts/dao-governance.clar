;; DAO Governance Smart Contract
;; Manages decentralized decision-making for subscription pool services

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_PROPOSAL (err u101))
(define-constant ERR_ALREADY_VOTED (err u102))
(define-constant ERR_PROPOSAL_EXPIRED (err u103))
(define-constant ERR_PROPOSAL_NOT_ACTIVE (err u104))
(define-constant ERR_INSUFFICIENT_TOKENS (err u105))
(define-constant ERR_INVALID_PARTICIPANT (err u106))

;; Data Variables
(define-data-var proposal-counter uint u0)
(define-data-var governance-token-supply uint u0)
(define-data-var voting-period uint u1008) ;; ~1 week in blocks
(define-data-var minimum-participation uint u10) ;; Minimum percentage for quorum

;; Data Maps
(define-map participants 
  principal 
  {
    tokens: uint,
    voting-power: uint,
    last-reward-claim: uint,
    total-votes-cast: uint
  }
)

(define-map proposals
  uint
  {
    proposer: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    proposal-type: (string-ascii 20), ;; "add-service", "remove-service", "adjust-share", "rule-change"
    target-service: (optional (string-ascii 50)),
    votes-for: uint,
    votes-against: uint,
    start-block: uint,
    end-block: uint,
    executed: bool,
    quorum-reached: bool
  }
)

(define-map votes
  {proposal-id: uint, voter: principal}
  {vote: bool, voting-power: uint, timestamp: uint}
)

(define-map services
  (string-ascii 50)
  {
    name: (string-ascii 100),
    cost: uint,
    active: bool,
    votes-received: uint,
    added-at: uint
  }
)

(define-map governance-tokens
  principal
  uint
)

(define-map reward-nfts
  uint
  {
    owner: principal,
    reward-type: (string-ascii 20),
    earned-at: uint
  }
)

(define-data-var nft-counter uint u0)

;; Public Functions

;; Join DAO as participant
(define-public (join-dao (initial-contribution uint))
  (let ((caller tx-sender))
    (asserts! (> initial-contribution u0) ERR_INSUFFICIENT_TOKENS)
    (map-set participants caller {
      tokens: initial-contribution,
      voting-power: initial-contribution,
      last-reward-claim: stacks-block-height,
      total-votes-cast: u0
    })
    (map-set governance-tokens caller initial-contribution)
    (var-set governance-token-supply (+ (var-get governance-token-supply) initial-contribution))
    (ok true)
  )
)

;; Create a new proposal
(define-public (create-proposal 
  (title (string-ascii 100))
  (description (string-ascii 500))
  (proposal-type (string-ascii 20))
  (target-service (optional (string-ascii 50)))
)
  (let (
    (proposal-id (+ (var-get proposal-counter) u1))
    (caller tx-sender)
    (participant-data (unwrap! (map-get? participants caller) ERR_INVALID_PARTICIPANT))
  )
    (asserts! (>= (get voting-power participant-data) u1) ERR_UNAUTHORIZED)
    
    (map-set proposals proposal-id {
      proposer: caller,
      title: title,
      description: description,
      proposal-type: proposal-type,
      target-service: target-service,
      votes-for: u0,
      votes-against: u0,
      start-block: stacks-block-height,
      end-block: (+ stacks-block-height (var-get voting-period)),
      executed: false,
      quorum-reached: false
    })
    
    (var-set proposal-counter proposal-id)
    (ok proposal-id)
  )
)

;; Vote on a proposal
(define-public (vote-on-proposal (proposal-id uint) (vote-for bool))
  (let (
    (caller tx-sender)
    (participant-data (unwrap! (map-get? participants caller) ERR_INVALID_PARTICIPANT))
    (proposal-data (unwrap! (map-get? proposals proposal-id) ERR_INVALID_PROPOSAL))
    (voting-power (get voting-power participant-data))
  )
    ;; Check if proposal is active
    (asserts! (<= (get start-block proposal-data) stacks-block-height) ERR_PROPOSAL_NOT_ACTIVE)
    (asserts! (>= (get end-block proposal-data) stacks-block-height) ERR_PROPOSAL_EXPIRED)
    
    ;; Check if already voted
    (asserts! (is-none (map-get? votes {proposal-id: proposal-id, voter: caller})) ERR_ALREADY_VOTED)
    
    ;; Record vote
    (map-set votes {proposal-id: proposal-id, voter: caller} {
      vote: vote-for,
      voting-power: voting-power,
      timestamp: stacks-block-height
    })
    
    ;; Update proposal vote counts
    (if vote-for
      (map-set proposals proposal-id 
        (merge proposal-data {votes-for: (+ (get votes-for proposal-data) voting-power)})
      )
      (map-set proposals proposal-id 
        (merge proposal-data {votes-against: (+ (get votes-against proposal-data) voting-power)})
      )
    )
    
    ;; Update participant vote count and reward them
    (map-set participants caller 
      (merge participant-data {total-votes-cast: (+ (get total-votes-cast participant-data) u1)})
    )
    
    ;; Mint reward token for voting
    (try! (mint-voting-reward caller))
    
    (ok true)
  )
)

;; Alternative approach using is-ok check
(define-public (execute-proposal-alt (proposal-id uint))
  (let (
    (proposal-data (unwrap! (map-get? proposals proposal-id) ERR_INVALID_PROPOSAL))
    (total-votes (+ (get votes-for proposal-data) (get votes-against proposal-data)))
    (total-supply (var-get governance-token-supply))
    (participation-rate (* (/ total-votes total-supply) u100))
  )
    ;; Check if voting period has ended
    (asserts! (< (get end-block proposal-data) stacks-block-height) ERR_PROPOSAL_NOT_ACTIVE)
    (asserts! (not (get executed proposal-data)) ERR_INVALID_PROPOSAL)
    
    ;; Check quorum
    (let ((quorum-reached (>= participation-rate (var-get minimum-participation))))
      (map-set proposals proposal-id 
        (merge proposal-data {
          executed: true,
          quorum-reached: quorum-reached
        })
      )
      
      ;; Execute proposal if passed and quorum reached
      (if (and quorum-reached (> (get votes-for proposal-data) (get votes-against proposal-data)))
        (let ((execution-result (execute-proposal-action proposal-data)))
          (if (is-ok execution-result)
            (ok true)
            execution-result ;; Return the error as-is
          )
        )
        (ok false)
      )
    )
  )
)


;; Add or remove services based on proposal
(define-private (execute-proposal-action (proposal-data {proposer: principal, title: (string-ascii 100), description: (string-ascii 500), proposal-type: (string-ascii 20), target-service: (optional (string-ascii 50)), votes-for: uint, votes-against: uint, start-block: uint, end-block: uint, executed: bool, quorum-reached: bool}))
  (let ((proposal-type (get proposal-type proposal-data)))
    (if (is-eq proposal-type "add-service")
      (match (get target-service proposal-data)
        service-name (begin
          (map-set services service-name {
            name: service-name,
            cost: u100, ;; Default cost, could be part of proposal
            active: true,
            votes-received: (get votes-for proposal-data),
            added-at: stacks-block-height
          })
          (ok true)
        )
        (ok false)
      )
      (if (is-eq proposal-type "remove-service")
        (match (get target-service proposal-data)
          service-name (begin
            (map-set services service-name 
              (merge (default-to {name: service-name, cost: u0, active: false, votes-received: u0, added-at: u0} 
                     (map-get? services service-name)) 
                     {active: false})
            )
            (ok true)
          )
          (ok false)
        )
        (ok true) ;; Other proposal types handled separately
      )
    )
  )
)

;; Mint reward NFT for active voting participation
(define-private (mint-voting-reward (recipient principal))
  (let (
    (nft-id (+ (var-get nft-counter) u1))
    (participant-data (unwrap! (map-get? participants recipient) ERR_INVALID_PARTICIPANT))
  )
    (map-set reward-nfts nft-id {
      owner: recipient,
      reward-type: "voting-participation",
      earned-at: stacks-block-height
    })
    (var-set nft-counter nft-id)
    
    ;; Award additional governance tokens for participation
    (let ((current-tokens (default-to u0 (map-get? governance-tokens recipient))))
      (map-set governance-tokens recipient (+ current-tokens u1))
      (var-set governance-token-supply (+ (var-get governance-token-supply) u1))
    )
    
    (ok nft-id)
  )
)

;; Claim accumulated rewards
(define-public (claim-rewards)
  (let (
    (caller tx-sender)
    (participant-data (unwrap! (map-get? participants caller) ERR_INVALID_PARTICIPANT))
    (votes-cast (get total-votes-cast participant-data))
    (reward-amount (/ votes-cast u10)) ;; 1 bonus token per 10 votes
  )
    (if (> reward-amount u0)
      (begin
        (map-set governance-tokens caller 
          (+ (default-to u0 (map-get? governance-tokens caller)) reward-amount)
        )
        (map-set participants caller 
          (merge participant-data {last-reward-claim: stacks-block-height})
        )
        (var-set governance-token-supply (+ (var-get governance-token-supply) reward-amount))
        (ok reward-amount)
      )
      (ok u0)
    )
  )
)

;; Read-only functions

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals proposal-id)
)

(define-read-only (get-participant (participant principal))
  (map-get? participants participant)
)

(define-read-only (get-service (service-name (string-ascii 50)))
  (map-get? services service-name)
)

(define-read-only (get-vote (proposal-id uint) (voter principal))
  (map-get? votes {proposal-id: proposal-id, voter: voter})
)

(define-read-only (get-governance-tokens (holder principal))
  (default-to u0 (map-get? governance-tokens holder))
)

(define-read-only (get-total-supply)
  (var-get governance-token-supply)
)

(define-read-only (get-active-proposals)
  (var-get proposal-counter)
)

;; Admin functions (can be removed or modified based on decentralization needs)

(define-public (update-voting-period (new-period uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set voting-period new-period)
    (ok true)
  )
)

(define-public (update-minimum-participation (new-minimum uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (<= new-minimum u100) ERR_INVALID_PROPOSAL)
    (var-set minimum-participation new-minimum)
    (ok true)
  )
)