(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ALREADY-REGISTERED (err u101))
(define-constant ERR-NOT-REGISTERED (err u102))
(define-constant ERR-INVALID-SCORE (err u103))
(define-constant ERR-ALREADY-VOTED (err u104))
(define-constant ERR-SELF-DELEGATION (err u105))
(define-constant ERR-INVALID-DELEGATE (err u106))
(define-constant ERR-INSUFFICIENT-REWARDS (err u107))
(define-constant ERR-INVALID-AMOUNT (err u108))

(define-data-var min-reviewer-stake uint u10000)
(define-data-var voting-period uint u144)
(define-data-var consensus-threshold uint u75)
(define-data-var reward-pool uint u0)
(define-data-var base-reward-per-review uint u1000)
(define-data-var withdrawal-cooldown uint u144)

(define-map reviewers 
  principal 
  {stake: uint, reputation: uint}
)

(define-map startups
  principal
  {
    name: (string-ascii 50),
    description: (string-ascii 256),
    total-score: uint,
    transparency-score: uint,
    roadmap-score: uint,
    tokenomics-score: uint,
    review-count: uint,
    registered-at: uint
  }
)

(define-map votes
  {reviewer: principal, startup: principal}
  {
    transparency: uint,
    roadmap: uint,
    tokenomics: uint,
    timestamp: uint
  }
)

(define-map delegations
  principal
  {
    delegate: principal,
    delegated-at: uint
  }
)

(define-map withdrawal-requests
  principal
  {
    requested-at: uint
  }
)

(define-map reward-balances
  principal
  {
    earned: uint,
    claimed: uint
  }
)

(define-read-only (get-startup-details (startup-id principal))
  (match (map-get? startups startup-id)
    startup (ok startup)
    (err ERR-NOT-REGISTERED)
  )
)

(define-read-only (get-reviewer-details (reviewer-id principal))
  (match (map-get? reviewers reviewer-id)
    reviewer (ok reviewer)
    (err ERR-NOT-REGISTERED)
  )
)

(define-read-only (get-delegation (delegator principal))
  (map-get? delegations delegator)
)

(define-read-only (get-effective-reviewer (original-reviewer principal))
  (match (map-get? delegations original-reviewer)
    delegation (get delegate delegation)
    original-reviewer
  )
)

(define-read-only (get-reward-balance (reviewer principal))
  (default-to {earned: u0, claimed: u0} (map-get? reward-balances reviewer))
)

(define-read-only (get-claimable-rewards (reviewer principal))
  (let ((balance (get-reward-balance reviewer)))
    (- (get earned balance) (get claimed balance))
  )
)

(define-read-only (get-reward-pool-balance)
  (var-get reward-pool)
)

(define-public (register-startup (name (string-ascii 50)) (description (string-ascii 256)))
  (let ((startup-exists (map-get? startups tx-sender)))
    (asserts! (is-none startup-exists) ERR-ALREADY-REGISTERED)
    (ok (map-set startups tx-sender
      {
        name: name,
        description: description,
        total-score: u0,
        transparency-score: u0,
        roadmap-score: u0,
        tokenomics-score: u0,
        review-count: u0,
        registered-at:  burn-block-height
      }
    ))
  )
)

(define-public (register-reviewer (stake uint))
  (let ((reviewer-exists (map-get? reviewers tx-sender)))
    (asserts! (is-none reviewer-exists) ERR-ALREADY-REGISTERED)
    (asserts! (>= stake (var-get min-reviewer-stake)) ERR-NOT-AUTHORIZED)
    (ok (map-set reviewers tx-sender
      {
        stake: stake,
        reputation: u100
      }
    ))
  )
)

(define-public (delegate-to (delegate-address principal))
  (let ((delegator-reviewer (unwrap! (map-get? reviewers tx-sender) ERR-NOT-AUTHORIZED))
        (delegate-reviewer (unwrap! (map-get? reviewers delegate-address) ERR-INVALID-DELEGATE)))
    (asserts! (not (is-eq tx-sender delegate-address)) ERR-SELF-DELEGATION)
    (ok (map-set delegations tx-sender
      {
        delegate: delegate-address,
        delegated-at: burn-block-height
      }
    ))
  )
)

(define-public (revoke-delegation)
  (let ((delegation-exists (unwrap! (map-get? delegations tx-sender) ERR-NOT-REGISTERED)))
    (ok (map-delete delegations tx-sender))
  )
)

(define-public (fund-reward-pool (amount uint))
  (begin
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (var-set reward-pool (+ (var-get reward-pool) amount))
    (ok amount)
  )
)

(define-public (claim-rewards)
  (let (
    (claimable (get-claimable-rewards tx-sender))
    (current-balance (get-reward-balance tx-sender))
  )
    (asserts! (> claimable u0) ERR-INSUFFICIENT-REWARDS)
    (asserts! (>= (var-get reward-pool) claimable) ERR-INSUFFICIENT-REWARDS)
    
    (var-set reward-pool (- (var-get reward-pool) claimable))
    (map-set reward-balances tx-sender
      {
        earned: (get earned current-balance),
        claimed: (get earned current-balance)
      }
    )
    (ok claimable)
  )
)

(define-public (submit-review 
    (startup-id principal)
    (transparency uint)
    (roadmap uint)
    (tokenomics uint))
  (let (
    (effective-reviewer (get-effective-reviewer tx-sender))
    (reviewer (unwrap! (map-get? reviewers effective-reviewer) ERR-NOT-AUTHORIZED))
    (startup (unwrap! (map-get? startups startup-id) ERR-NOT-REGISTERED))
    (vote-key {reviewer: effective-reviewer, startup: startup-id})
  )
    (asserts! (is-none (map-get? votes vote-key)) ERR-ALREADY-VOTED)
    (asserts! (and (<= transparency u100) (<= roadmap u100) (<= tokenomics u100)) ERR-INVALID-SCORE)
    
    (map-set votes vote-key
      {
        transparency: transparency,
        roadmap: roadmap,
        tokenomics: tokenomics,
        timestamp: burn-block-height
      }
    )
    
    (try! (update-startup-scores startup-id transparency roadmap tokenomics))
    (distribute-review-reward effective-reviewer)
  )
)

(define-private (update-startup-scores
    (startup-id principal)
    (transparency uint)
    (roadmap uint)
    (tokenomics uint))
  (match (map-get? startups startup-id)
    startup (ok (map-set startups startup-id
      {
        name: (get name startup),
        description: (get description startup),
        total-score: (/ (+ transparency roadmap tokenomics) u3),
        transparency-score: transparency,
        roadmap-score: roadmap,
        tokenomics-score: tokenomics,
        review-count: (+ (get review-count startup) u1),
        registered-at: (get registered-at startup)
      }))
    ERR-NOT-REGISTERED
  )
)

(define-private (distribute-review-reward (reviewer principal))
  (let (
    (reviewer-data (unwrap-panic (map-get? reviewers reviewer)))
    (base-reward (var-get base-reward-per-review))
    (reputation-multiplier (/ (get reputation reviewer-data) u100))
    (final-reward (* base-reward reputation-multiplier))
    (current-balance (get-reward-balance reviewer))
  )
    (if (> (var-get reward-pool) final-reward)
      (ok (map-set reward-balances reviewer
        {
          earned: (+ (get earned current-balance) final-reward),
          claimed: (get claimed current-balance)
        }
      ))
      (ok false)
    )
  )
)

(define-public (update-startup-info (name (string-ascii 50)) (description (string-ascii 256)))
  (let ((startup (unwrap! (map-get? startups tx-sender) ERR-NOT-REGISTERED)))
    (ok (map-set startups tx-sender
      {
        name: name,
        description: description,
        total-score: (get total-score startup),
        transparency-score: (get transparency-score startup),
        roadmap-score: (get roadmap-score startup),
        tokenomics-score: (get tokenomics-score startup),
        review-count: (get review-count startup),
        registered-at: (get registered-at startup)
      }
    ))
  )
)

(define-public (request-withdrawal)
  (let ((reviewer (unwrap! (map-get? reviewers tx-sender) ERR-NOT-AUTHORIZED))
        (delegation (map-get? delegations tx-sender)))
    (asserts! (is-none delegation) ERR-NOT-AUTHORIZED)
    (ok (map-set withdrawal-requests tx-sender
      {
        requested-at: burn-block-height
      }
    ))
  )
)

(define-public (withdraw-stake)
  (let ((request (unwrap! (map-get? withdrawal-requests tx-sender) ERR-NOT-AUTHORIZED))
        (time-passed (- burn-block-height (get requested-at request))))
    (asserts! (>= time-passed (var-get withdrawal-cooldown)) ERR-NOT-AUTHORIZED)
    (map-delete reviewers tx-sender)
    (map-delete withdrawal-requests tx-sender)
    (ok true)
  )
)
