;; Binary Batch - Supply Chain Management Smart Contract
;; A simplified implementation for batch tracking and compliance

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-state (err u103))
(define-constant err-already-exists (err u104))

;; Batch states
(define-constant state-created u0)
(define-constant state-in-transit u1)
(define-constant state-quality-check u2)
(define-constant state-approved u3)
(define-constant state-recalled u4)

;; Data Variables
(define-data-var batch-counter uint u0)

;; Data Maps
(define-map batches
  { batch-id: uint }
  {
    fingerprint: (buff 32),
    manufacturer: principal,
    current-state: uint,
    created-at: uint,
    updated-at: uint,
    location: (string-ascii 100),
    quality-score: uint,
    compliance-verified: bool
  }
)

(define-map batch-history
  { batch-id: uint, sequence: uint }
  {
    state: uint,
    timestamp: uint,
    actor: principal,
    notes: (string-ascii 256)
  }
)

(define-map authorized-participants
  { participant: principal }
  {
    role: (string-ascii 50),
    authorized: bool
  }
)

(define-map compliance-checks
  { batch-id: uint }
  {
    temperature-compliant: bool,
    documentation-complete: bool,
    quality-passed: bool,
    verified-by: (optional principal),
    verified-at: (optional uint)
  }
)

;; Authorization Functions
(define-public (authorize-participant (participant principal) (role (string-ascii 50)))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (ok (map-set authorized-participants
      { participant: participant }
      { role: role, authorized: true }
    ))
  )
)

(define-public (revoke-participant (participant principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (ok (map-set authorized-participants
      { participant: participant }
      { role: "", authorized: false }
    ))
  )
)

(define-read-only (is-authorized (participant principal))
  (default-to false
    (get authorized (map-get? authorized-participants { participant: participant }))
  )
)

;; Batch Management Functions
(define-public (create-batch 
  (fingerprint (buff 32))
  (location (string-ascii 100))
  (quality-score uint))
  (let
    (
      (new-batch-id (+ (var-get batch-counter) u1))
      (current-time block-height)
    )
    (asserts! (is-authorized tx-sender) err-unauthorized)
    (asserts! (<= quality-score u100) err-invalid-state)
    
    (map-set batches
      { batch-id: new-batch-id }
      {
        fingerprint: fingerprint,
        manufacturer: tx-sender,
        current-state: state-created,
        created-at: current-time,
        updated-at: current-time,
        location: location,
        quality-score: quality-score,
        compliance-verified: false
      }
    )
    
    (map-set batch-history
      { batch-id: new-batch-id, sequence: u0 }
      {
        state: state-created,
        timestamp: current-time,
        actor: tx-sender,
        notes: "Batch created"
      }
    )
    
    (var-set batch-counter new-batch-id)
    (ok new-batch-id)
  )
)

(define-public (update-batch-state 
  (batch-id uint)
  (new-state uint)
  (new-location (string-ascii 100))
  (notes (string-ascii 256)))
  (let
    (
      (batch (unwrap! (map-get? batches { batch-id: batch-id }) err-not-found))
      (current-time block-height)
      (history-count (get-history-count batch-id))
    )
    (asserts! (is-authorized tx-sender) err-unauthorized)
    (asserts! (<= new-state state-recalled) err-invalid-state)
    
    (map-set batches
      { batch-id: batch-id }
      (merge batch {
        current-state: new-state,
        updated-at: current-time,
        location: new-location
      })
    )
    
    (map-set batch-history
      { batch-id: batch-id, sequence: (+ history-count u1) }
      {
        state: new-state,
        timestamp: current-time,
        actor: tx-sender,
        notes: notes
      }
    )
    
    (ok true)
  )
)

(define-public (record-compliance-check
  (batch-id uint)
  (temp-compliant bool)
  (docs-complete bool)
  (quality-pass bool))
  (let
    (
      (batch (unwrap! (map-get? batches { batch-id: batch-id }) err-not-found))
      (all-passed (and temp-compliant (and docs-complete quality-pass)))
    )
    (asserts! (is-authorized tx-sender) err-unauthorized)
    
    (map-set compliance-checks
      { batch-id: batch-id }
      {
        temperature-compliant: temp-compliant,
        documentation-complete: docs-complete,
        quality-passed: quality-pass,
        verified-by: (some tx-sender),
        verified-at: (some block-height)
      }
    )
    
    (map-set batches
      { batch-id: batch-id }
      (merge batch { compliance-verified: all-passed })
    )
    
    (ok all-passed)
  )
)

(define-public (recall-batch (batch-id uint) (reason (string-ascii 256)))
  (let
    (
      (batch (unwrap! (map-get? batches { batch-id: batch-id }) err-not-found))
    )
    (asserts! (or (is-eq tx-sender contract-owner) 
                  (is-eq tx-sender (get manufacturer batch))) 
              err-unauthorized)
    
    (try! (update-batch-state batch-id state-recalled 
                              (get location batch) reason))
    (ok true)
  )
)

;; Read-only Functions
(define-read-only (get-batch (batch-id uint))
  (ok (map-get? batches { batch-id: batch-id }))
)

(define-read-only (get-batch-state (batch-id uint))
  (ok (get current-state (unwrap! (map-get? batches { batch-id: batch-id }) err-not-found)))
)

(define-read-only (get-compliance-status (batch-id uint))
  (ok (map-get? compliance-checks { batch-id: batch-id }))
)

(define-read-only (get-batch-history (batch-id uint) (sequence uint))
  (ok (map-get? batch-history { batch-id: batch-id, sequence: sequence }))
)

(define-read-only (get-total-batches)
  (ok (var-get batch-counter))
)

;; Private helper function to count history entries
(define-private (get-history-count (batch-id uint))
  (+ 
    (if (is-some (map-get? batch-history { batch-id: batch-id, sequence: u0 })) u1 u0)
    (if (is-some (map-get? batch-history { batch-id: batch-id, sequence: u1 })) u1 u0)
    (if (is-some (map-get? batch-history { batch-id: batch-id, sequence: u2 })) u1 u0)
    (if (is-some (map-get? batch-history { batch-id: batch-id, sequence: u3 })) u1 u0)
    (if (is-some (map-get? batch-history { batch-id: batch-id, sequence: u4 })) u1 u0)
    (if (is-some (map-get? batch-history { batch-id: batch-id, sequence: u5 })) u1 u0)
    (if (is-some (map-get? batch-history { batch-id: batch-id, sequence: u6 })) u1 u0)
    (if (is-some (map-get? batch-history { batch-id: batch-id, sequence: u7 })) u1 u0)
    (if (is-some (map-get? batch-history { batch-id: batch-id, sequence: u8 })) u1 u0)
    (if (is-some (map-get? batch-history { batch-id: batch-id, sequence: u9 })) u1 u0)
  )
)