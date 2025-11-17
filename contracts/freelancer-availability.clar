(define-constant ERR_NOT_AUTHORIZED (err u200))
(define-constant ERR_INVALID_CAPACITY (err u201))
(define-constant ERR_INVALID_STATUS (err u202))
(define-constant ERR_UNAVAILABLE (err u203))

(define-map freelancer-availability
  { freelancer: principal }
  {
    status: (string-ascii 20),
    max-concurrent-jobs: uint,
    current-active-jobs: uint,
    available-from-block: uint,
    hourly-rate: uint,
    last-updated: uint
  }
)

(define-map availability-history
  { freelancer: principal, timestamp: uint }
  {
    status: (string-ascii 20),
    block-height: uint
  }
)

(define-public (set-availability 
  (status (string-ascii 20))
  (max-concurrent uint)
  (available-from uint)
  (hourly-rate uint))
  (let
    (
      (current-block stacks-block-height)
      (current-data (default-to
        { status: "unavailable", max-concurrent-jobs: u0, current-active-jobs: u0,
          available-from-block: u0, hourly-rate: u0, last-updated: u0 }
        (map-get? freelancer-availability { freelancer: tx-sender })
      ))
    )
    (asserts! (> max-concurrent u0) ERR_INVALID_CAPACITY)
    (asserts! (or (is-eq status "available") (is-eq status "busy") (is-eq status "unavailable")) ERR_INVALID_STATUS)
    (map-set availability-history 
      { freelancer: tx-sender, timestamp: current-block }
      { status: status, block-height: current-block }
    )
    (ok (map-set freelancer-availability { freelancer: tx-sender }
      {
        status: status,
        max-concurrent-jobs: max-concurrent,
        current-active-jobs: (get current-active-jobs current-data),
        available-from-block: available-from,
        hourly-rate: hourly-rate,
        last-updated: current-block
      }
    ))
  )
)

(define-public (increment-active-jobs (freelancer principal))
  (let
    (
      (availability (unwrap! (map-get? freelancer-availability { freelancer: freelancer }) ERR_UNAVAILABLE))
      (new-count (+ (get current-active-jobs availability) u1))
      (new-status (if (>= new-count (get max-concurrent-jobs availability)) "busy" "available"))
    )
    (ok (map-set freelancer-availability { freelancer: freelancer }
      (merge availability { 
        current-active-jobs: new-count,
        status: new-status
      })
    ))
  )
)

(define-public (decrement-active-jobs (freelancer principal))
  (let
    (
      (availability (unwrap! (map-get? freelancer-availability { freelancer: freelancer }) ERR_UNAVAILABLE))
      (current-count (get current-active-jobs availability))
      (new-count (if (> current-count u0) (- current-count u1) u0))
    )
    (ok (map-set freelancer-availability { freelancer: freelancer }
      (merge availability { 
        current-active-jobs: new-count,
        status: "available"
      })
    ))
  )
)

(define-read-only (get-availability (freelancer principal))
  (map-get? freelancer-availability { freelancer: freelancer })
)

(define-read-only (is-available (freelancer principal))
  (match (map-get? freelancer-availability { freelancer: freelancer })
    availability-data (and 
      (is-eq (get status availability-data) "available")
      (< (get current-active-jobs availability-data) (get max-concurrent-jobs availability-data))
      (<= (get available-from-block availability-data) stacks-block-height)
    )
    false
  )
)

(define-read-only (get-availability-history (freelancer principal) (timestamp uint))
  (map-get? availability-history { freelancer: freelancer, timestamp: timestamp })
)
