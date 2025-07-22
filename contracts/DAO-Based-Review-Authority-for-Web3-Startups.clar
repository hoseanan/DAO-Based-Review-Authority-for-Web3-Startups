(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ALREADY-REGISTERED (err u101))
(define-constant ERR-NOT-REGISTERED (err u102))
(define-constant ERR-INVALID-SCORE (err u103))
(define-constant ERR-ALREADY-VOTED (err u104))

(define-data-var min-reviewer-stake uint u10000)
(define-data-var voting-period uint u144)
(define-data-var consensus-threshold uint u75)

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

(define-public (submit-review 
    (startup-id principal)
    (transparency uint)
    (roadmap uint)
    (tokenomics uint))
  (let (
    (reviewer (unwrap! (map-get? reviewers tx-sender) ERR-NOT-AUTHORIZED))
    (startup (unwrap! (map-get? startups startup-id) ERR-NOT-REGISTERED))
    (vote-key {reviewer: tx-sender, startup: startup-id})
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
    
    (update-startup-scores startup-id transparency roadmap tokenomics)
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