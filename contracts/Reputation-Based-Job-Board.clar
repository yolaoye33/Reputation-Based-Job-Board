(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_JOB_NOT_FOUND (err u101))
(define-constant ERR_INVALID_AMOUNT (err u102))
(define-constant ERR_JOB_ALREADY_ASSIGNED (err u103))
(define-constant ERR_JOB_NOT_ASSIGNED (err u104))
(define-constant ERR_INSUFFICIENT_FUNDS (err u105))
(define-constant ERR_INVALID_RATING (err u106))
(define-constant ERR_ALREADY_RATED (err u107))
(define-constant ERR_CANNOT_RATE_SELF (err u108))
(define-constant ERR_JOB_NOT_COMPLETED (err u109))

(define-constant ERR_DEADLINE_NOT_REACHED (err u110))
(define-constant ERR_JOB_ALREADY_EXPIRED (err u111))
(define-constant DEFAULT_DEADLINE_BLOCKS u100)

(define-constant ERR_DISPUTE_EXISTS (err u112))
(define-constant ERR_DISPUTE_NOT_FOUND (err u113))
(define-constant ERR_DISPUTE_WINDOW_CLOSED (err u114))
(define-constant MAX_DISPUTE_WINDOW u1000)

(define-data-var job-counter uint u0)

(define-map jobs
  { job-id: uint }
  {
    employer: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    budget: uint,
    freelancer: (optional principal),
    status: (string-ascii 20),
    created-at: uint,
    completed-at: (optional uint)
  }
)

(define-map user-profiles
  { user: principal }
  {
    total-jobs: uint,
    completed-jobs: uint,
    total-rating: uint,
    rating-count: uint,
    total-earned: uint,
    total-spent: uint,
    reputation-score: uint
  }
)

(define-map job-applications
  { job-id: uint, freelancer: principal }
  {
    proposal: (string-ascii 300),
    applied-at: uint
  }
)

(define-map job-ratings
  { job-id: uint, rater: principal }
  {
    rating: uint,
    review: (string-ascii 200),
    rated-at: uint
  }
)

(define-map escrow-funds
  { job-id: uint }
  { amount: uint }
)

(define-private (get-next-job-id)
  (begin
    (var-set job-counter (+ (var-get job-counter) u1))
    (var-get job-counter)
  )
)

(define-private (calculate-reputation-score (total-rating uint) (rating-count uint) (completed-jobs uint))
  (if (is-eq rating-count u0)
    u0
    (let
      (
        (avg-rating (/ total-rating rating-count))
        (completion-bonus (if (>= completed-jobs u10) u20 (/ completed-jobs u2)))
      )
      (+ (* avg-rating u10) completion-bonus)
    )
  )
)

(define-private (update-user-reputation (user principal))
  (let
    (
      (profile (default-to 
        { total-jobs: u0, completed-jobs: u0, total-rating: u0, rating-count: u0, 
          total-earned: u0, total-spent: u0, reputation-score: u0 }
        (map-get? user-profiles { user: user })
      ))
    )
    (map-set user-profiles { user: user }
      (merge profile {
        reputation-score: (calculate-reputation-score 
          (get total-rating profile)
          (get rating-count profile)
          (get completed-jobs profile)
        )
      })
    )
  )
)

(define-public (create-profile)
  (ok (map-set user-profiles { user: tx-sender }
    { total-jobs: u0, completed-jobs: u0, total-rating: u0, rating-count: u0,
      total-earned: u0, total-spent: u0, reputation-score: u0 }
  ))
)

(define-public (post-job (title (string-ascii 100)) (description (string-ascii 500)) (budget uint))
  (let
    (
      (job-id (get-next-job-id))
      (current-block stacks-block-height)
    )
    (asserts! (> budget u0) ERR_INVALID_AMOUNT)
    (try! (stx-transfer? budget tx-sender (as-contract tx-sender)))
    (map-set jobs { job-id: job-id }
      {
        employer: tx-sender,
        title: title,
        description: description,
        budget: budget,
        freelancer: none,
        status: "open",
        created-at: current-block,
        completed-at: none
      }
    )
    (map-set escrow-funds { job-id: job-id } { amount: budget })
    (let
      (
        (employer-profile (default-to 
          { total-jobs: u0, completed-jobs: u0, total-rating: u0, rating-count: u0,
            total-earned: u0, total-spent: u0, reputation-score: u0 }
          (map-get? user-profiles { user: tx-sender })
        ))
      )
      (map-set user-profiles { user: tx-sender }
        (merge employer-profile {
          total-jobs: (+ (get total-jobs employer-profile) u1),
          total-spent: (+ (get total-spent employer-profile) budget)
        })
      )
      (update-user-reputation tx-sender)
      (ok job-id)
    )
  )
)

(define-public (apply-for-job (job-id uint) (proposal (string-ascii 300)))
  (let
    (
      (job (unwrap! (map-get? jobs { job-id: job-id }) ERR_JOB_NOT_FOUND))
      (current-block stacks-block-height)
    )
    (asserts! (is-eq (get status job) "open") ERR_JOB_ALREADY_ASSIGNED)
    (asserts! (not (is-eq tx-sender (get employer job))) ERR_CANNOT_RATE_SELF)
    (ok (map-set job-applications { job-id: job-id, freelancer: tx-sender }
      {
        proposal: proposal,
        applied-at: current-block
      }
    ))
  )
)

(define-public (assign-job (job-id uint) (freelancer principal))
  (let
    (
      (job (unwrap! (map-get? jobs { job-id: job-id }) ERR_JOB_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get employer job)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status job) "open") ERR_JOB_ALREADY_ASSIGNED)
    (ok (map-set jobs { job-id: job-id }
      (merge job {
        freelancer: (some freelancer),
        status: "assigned"
      })
    ))
  )
)

(define-public (complete-job (job-id uint))
  (let
    (
      (job (unwrap! (map-get? jobs { job-id: job-id }) ERR_JOB_NOT_FOUND))
      (current-block stacks-block-height)
    )
    (asserts! (is-eq tx-sender (get employer job)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status job) "assigned") ERR_JOB_NOT_ASSIGNED)
    (let
      (
        (freelancer (unwrap! (get freelancer job) ERR_JOB_NOT_ASSIGNED))
        (escrow (unwrap! (map-get? escrow-funds { job-id: job-id }) ERR_INSUFFICIENT_FUNDS))
        (amount (get amount escrow))
      )
      (try! (as-contract (stx-transfer? amount tx-sender freelancer)))
      (map-delete escrow-funds { job-id: job-id })
      (map-set jobs { job-id: job-id }
        (merge job {
          status: "completed",
          completed-at: (some current-block)
        })
      )
      (let
        (
          (freelancer-profile (default-to 
            { total-jobs: u0, completed-jobs: u0, total-rating: u0, rating-count: u0,
              total-earned: u0, total-spent: u0, reputation-score: u0 }
            (map-get? user-profiles { user: freelancer })
          ))
          (employer-profile (default-to 
            { total-jobs: u0, completed-jobs: u0, total-rating: u0, rating-count: u0,
              total-earned: u0, total-spent: u0, reputation-score: u0 }
            (map-get? user-profiles { user: tx-sender })
          ))
        )
        (map-set user-profiles { user: freelancer }
          (merge freelancer-profile {
            completed-jobs: (+ (get completed-jobs freelancer-profile) u1),
            total-earned: (+ (get total-earned freelancer-profile) amount)
          })
        )
        (map-set user-profiles { user: tx-sender }
          (merge employer-profile {
            completed-jobs: (+ (get completed-jobs employer-profile) u1)
          })
        )
        (update-user-reputation freelancer)
        (update-user-reputation tx-sender)
        (ok true)
      )
    )
  )
)
(define-public (rate-user (job-id uint) (rated-user principal) (rating uint) (review (string-ascii 200)))
  (let
    (
      (job (unwrap! (map-get? jobs { job-id: job-id }) ERR_JOB_NOT_FOUND))
      (current-block stacks-block-height)
    )
    (asserts! (and (>= rating u1) (<= rating u5)) ERR_INVALID_RATING)
    (asserts! (is-eq (get status job) "completed") ERR_JOB_NOT_COMPLETED)
    (asserts! (not (is-eq tx-sender rated-user)) ERR_CANNOT_RATE_SELF)
    (asserts! (is-none (map-get? job-ratings { job-id: job-id, rater: tx-sender })) ERR_ALREADY_RATED)
    (asserts! (or 
      (and (is-eq tx-sender (get employer job)) (is-eq rated-user (unwrap! (get freelancer job) ERR_JOB_NOT_ASSIGNED)))
      (and (is-eq tx-sender (unwrap! (get freelancer job) ERR_JOB_NOT_ASSIGNED)) (is-eq rated-user (get employer job)))
    ) ERR_NOT_AUTHORIZED)
    (map-set job-ratings { job-id: job-id, rater: tx-sender }
      {
        rating: rating,
        review: review,
        rated-at: current-block
      }
    )
    (let
      (
        (user-profile (default-to 
          { total-jobs: u0, completed-jobs: u0, total-rating: u0, rating-count: u0,
            total-earned: u0, total-spent: u0, reputation-score: u0 }
          (map-get? user-profiles { user: rated-user })
        ))
      )
      (map-set user-profiles { user: rated-user }
        (merge user-profile {
          total-rating: (+ (get total-rating user-profile) rating),
          rating-count: (+ (get rating-count user-profile) u1)
        })
      )
      (update-user-reputation rated-user)
      (ok true)
    )
  )
)
(define-read-only (get-job (job-id uint))
  (map-get? jobs { job-id: job-id })
)

(define-read-only (get-user-profile (user principal))
  (map-get? user-profiles { user: user })
)

(define-read-only (get-job-application (job-id uint) (freelancer principal))
  (map-get? job-applications { job-id: job-id, freelancer: freelancer })
)

(define-read-only (get-job-rating (job-id uint) (rater principal))
  (map-get? job-ratings { job-id: job-id, rater: rater })
)

(define-read-only (get-total-jobs)
  (var-get job-counter)
)

(define-map job-deadlines
  { job-id: uint }
  { deadline-block: uint }
)

(define-private (is-job-expired (job-id uint))
  (match (map-get? job-deadlines { job-id: job-id })
    deadline-data (>= stacks-block-height (get deadline-block deadline-data))
    false
  )
)

(define-public (set-job-deadline (job-id uint) (deadline-blocks uint))
  (let
    (
      (job (unwrap! (map-get? jobs { job-id: job-id }) ERR_JOB_NOT_FOUND))
      (deadline-block (+ stacks-block-height deadline-blocks))
    )
    (asserts! (is-eq tx-sender (get employer job)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status job) "open") ERR_JOB_ALREADY_ASSIGNED)
    (asserts! (> deadline-blocks u0) ERR_INVALID_AMOUNT)
    (ok (map-set job-deadlines { job-id: job-id } { deadline-block: deadline-block }))
  )
)

(define-public (claim-expired-refund (job-id uint))
  (let
    (
      (job (unwrap! (map-get? jobs { job-id: job-id }) ERR_JOB_NOT_FOUND))
      (escrow (unwrap! (map-get? escrow-funds { job-id: job-id }) ERR_INSUFFICIENT_FUNDS))
    )
    (asserts! (is-eq tx-sender (get employer job)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status job) "open") ERR_JOB_ALREADY_ASSIGNED)
    (asserts! (is-job-expired job-id) ERR_DEADLINE_NOT_REACHED)
    (let
      (
        (refund-amount (get amount escrow))
      )
      (try! (as-contract (stx-transfer? refund-amount tx-sender (get employer job))))
      (map-delete escrow-funds { job-id: job-id })
      (map-delete job-deadlines { job-id: job-id })
      (map-set jobs { job-id: job-id }
        (merge job { status: "expired" })
      )
      (ok refund-amount)
    )
  )
)

(define-read-only (get-job-deadline (job-id uint))
  (map-get? job-deadlines { job-id: job-id })
)

(define-read-only (is-job-deadline-reached (job-id uint))
  (is-job-expired job-id)
)

(define-map job-categories
  { job-id: uint }
  { 
    category: (string-ascii 50),
    skills: (list 5 (string-ascii 30)),
    difficulty-level: uint
  }
)

(define-map category-jobs
  { category: (string-ascii 50) }
  { job-ids: (list 100 uint) }
)

(define-map skill-jobs
  { skill: (string-ascii 30) }
  { job-ids: (list 200 uint) }
)

(define-private (add-job-to-category (job-id uint) (category (string-ascii 50)))
  (let
    (
      (existing-jobs (default-to 
        { job-ids: (list) } 
        (map-get? category-jobs { category: category })
      ))
      (updated-list (unwrap! (as-max-len? (append (get job-ids existing-jobs) job-id) u100) (err u999)))
    )
    (ok (map-set category-jobs { category: category }
      { job-ids: updated-list }
    ))
  )
)

(define-private (add-job-to-skills (job-id uint) (skills (list 5 (string-ascii 30))))
  (fold add-job-to-skill skills (ok job-id))
)

(define-private (add-job-to-skill (skill (string-ascii 30)) (result (response uint uint)))
  (let
    (
      (job-id (try! result))
      (existing-jobs (default-to 
        { job-ids: (list) } 
        (map-get? skill-jobs { skill: skill })
      ))
    )
    (map-set skill-jobs { skill: skill }
      { job-ids: (unwrap! (as-max-len? (append (get job-ids existing-jobs) job-id) u200) (err u999)) }
    )
    (ok job-id)
  )
)

(define-public (post-categorized-job 
  (title (string-ascii 100)) 
  (description (string-ascii 500)) 
  (budget uint) 
  (category (string-ascii 50)) 
  (skills (list 5 (string-ascii 30))) 
  (difficulty-level uint))
  (let
    (
      (job-creation-result (try! (post-job title description budget)))
      (job-id job-creation-result)
    )
    (asserts! (and (>= difficulty-level u1) (<= difficulty-level u5)) ERR_INVALID_RATING)
    (map-set job-categories { job-id: job-id }
      {
        category: category,
        skills: skills,
        difficulty-level: difficulty-level
      }
    )
    (try! (add-job-to-category job-id category))
    (try! (add-job-to-skills job-id skills))
    (ok job-id)
  )
)

(define-read-only (get-job-category (job-id uint))
  (map-get? job-categories { job-id: job-id })
)

(define-read-only (get-jobs-by-category (category (string-ascii 50)))
  (map-get? category-jobs { category: category })
)

(define-read-only (get-jobs-by-skill (skill (string-ascii 30)))
  (map-get? skill-jobs { skill: skill })
)


(define-map job-milestones
  { job-id: uint, milestone-id: uint }
  {
    description: (string-ascii 200),
    amount: uint,
    status: (string-ascii 20),
    created-at: uint,
    completed-at: (optional uint)
  }
)

(define-map milestone-counters
  { job-id: uint }
  { count: uint }
)

(define-private (get-next-milestone-id (job-id uint))
  (let
    (
      (current-count (default-to { count: u0 } (map-get? milestone-counters { job-id: job-id })))
      (new-count (+ (get count current-count) u1))
    )
    (map-set milestone-counters { job-id: job-id } { count: new-count })
    new-count
  )
)

(define-private (sum-amounts (amount uint) (total uint))
  (+ total amount)
)

(define-public (create-milestone-job (title (string-ascii 100)) (description (string-ascii 500)) (milestone-descriptions (list 5 (string-ascii 200))) (milestone-amounts (list 5 uint)))
  (let
    (
      (total-budget (fold sum-amounts milestone-amounts u0))
      (job-creation-result (try! (post-job title description total-budget)))
      (job-id job-creation-result)
      (current-block stacks-block-height)
    )
    (asserts! (is-eq (len milestone-descriptions) (len milestone-amounts)) ERR_INVALID_AMOUNT)
    (try! (create-milestones-batch job-id milestone-descriptions milestone-amounts current-block))
    (ok job-id)
  )
)

(define-private (create-milestones-batch (job-id uint) (descriptions (list 5 (string-ascii 200))) (amounts (list 5 uint)) (block uint))
  (begin
    (asserts! (> (len descriptions) u0) ERR_INVALID_AMOUNT)
    (let
      (
        (desc-1 (default-to "" (element-at descriptions u0)))
        (desc-2 (default-to "" (element-at descriptions u1)))
        (desc-3 (default-to "" (element-at descriptions u2)))
        (amount-1 (default-to u0 (element-at amounts u0)))
        (amount-2 (default-to u0 (element-at amounts u1)))
        (amount-3 (default-to u0 (element-at amounts u2)))
      )
      (if (> (len descriptions) u0) (map-set job-milestones { job-id: job-id, milestone-id: (get-next-milestone-id job-id) }
        { description: desc-1, amount: amount-1, status: "pending", created-at: block, completed-at: none }) true)
      (if (> (len descriptions) u1) (map-set job-milestones { job-id: job-id, milestone-id: (get-next-milestone-id job-id) }
        { description: desc-2, amount: amount-2, status: "pending", created-at: block, completed-at: none }) true)
      (if (> (len descriptions) u2) (map-set job-milestones { job-id: job-id, milestone-id: (get-next-milestone-id job-id) }
        { description: desc-3, amount: amount-3, status: "pending", created-at: block, completed-at: none }) true)
      (ok true)
    )
  )
)

(define-public (complete-milestone (job-id uint) (milestone-id uint))
  (let
    (
      (job (unwrap! (map-get? jobs { job-id: job-id }) ERR_JOB_NOT_FOUND))
      (milestone (unwrap! (map-get? job-milestones { job-id: job-id, milestone-id: milestone-id }) ERR_JOB_NOT_FOUND))
      (freelancer (unwrap! (get freelancer job) ERR_JOB_NOT_ASSIGNED))
      (current-block stacks-block-height)
    )
    (asserts! (is-eq tx-sender (get employer job)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status job) "assigned") ERR_JOB_NOT_ASSIGNED)
    (asserts! (is-eq (get status milestone) "pending") ERR_JOB_ALREADY_ASSIGNED)
    (try! (as-contract (stx-transfer? (get amount milestone) tx-sender freelancer)))
    (map-set job-milestones { job-id: job-id, milestone-id: milestone-id }
      (merge milestone {
        status: "completed",
        completed-at: (some current-block)
      })
    )
    (ok true)
  )
)

(define-read-only (get-job-milestone (job-id uint) (milestone-id uint))
  (map-get? job-milestones { job-id: job-id, milestone-id: milestone-id })
)

(define-read-only (get-milestone-count (job-id uint))
  (map-get? milestone-counters { job-id: job-id })
)

(define-map job-disputes
  { job-id: uint }
  {
    initiator: principal,
    reason: (string-ascii 300),
    initiated-at: uint,
    status: (string-ascii 20)
  }
)

(define-public (initiate-dispute (job-id uint) (reason (string-ascii 300)))
  (let
    (
      (job (unwrap! (map-get? jobs { job-id: job-id }) ERR_JOB_NOT_FOUND))
      (current-block stacks-block-height)
    )
    (asserts! (is-eq (get status job) "assigned") ERR_JOB_NOT_ASSIGNED)
    (asserts! (or (is-eq tx-sender (get employer job)) 
                  (is-eq tx-sender (unwrap! (get freelancer job) ERR_JOB_NOT_ASSIGNED))) ERR_NOT_AUTHORIZED)
    (asserts! (is-none (map-get? job-disputes { job-id: job-id })) ERR_DISPUTE_EXISTS)
    (ok (map-set job-disputes { job-id: job-id }
      {
        initiator: tx-sender,
        reason: reason,
        initiated-at: current-block,
        status: "open"
      }
    ))
  )
)

(define-public (resolve-dispute (job-id uint))
  (let
    (
      (job (unwrap! (map-get? jobs { job-id: job-id }) ERR_JOB_NOT_FOUND))
      (dispute (unwrap! (map-get? job-disputes { job-id: job-id }) ERR_DISPUTE_NOT_FOUND))
      (escrow (unwrap! (map-get? escrow-funds { job-id: job-id }) ERR_INSUFFICIENT_FUNDS))
      (freelancer (unwrap! (get freelancer job) ERR_JOB_NOT_ASSIGNED))
      (employer (get employer job))
      (current-block stacks-block-height)
      (blocks-elapsed (- current-block (get created-at job)))
      (refund-percentage (calculate-refund-percentage blocks-elapsed))
      (refund-amount (/ (* (get amount escrow) refund-percentage) u100))
      (freelancer-payment (- (get amount escrow) refund-amount))
    )
    (asserts! (is-eq (get status dispute) "open") ERR_JOB_ALREADY_ASSIGNED)
    (asserts! (< (- current-block (get initiated-at dispute)) MAX_DISPUTE_WINDOW) ERR_DISPUTE_WINDOW_CLOSED)
    (try! (as-contract (stx-transfer? refund-amount tx-sender employer)))
    (try! (as-contract (stx-transfer? freelancer-payment tx-sender freelancer)))
    (map-delete escrow-funds { job-id: job-id })
    (map-set job-disputes { job-id: job-id }
      (merge dispute { status: "resolved" })
    )
    (map-set jobs { job-id: job-id }
      (merge job { status: "disputed" })
    )
    (ok { refund: refund-amount, payment: freelancer-payment })
  )
)

(define-private (calculate-refund-percentage (blocks-elapsed uint))
  (if (<= blocks-elapsed u50)
    u90
    (if (<= blocks-elapsed u150)
      u70
      (if (<= blocks-elapsed u300)
        u50
        u30
      )
    )
  )
)

(define-read-only (get-dispute (job-id uint))
  (map-get? job-disputes { job-id: job-id })
)