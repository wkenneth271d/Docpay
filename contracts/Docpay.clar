(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_CONSULTATION_NOT_FOUND (err u101))
(define-constant ERR_INVALID_STATUS (err u102))
(define-constant ERR_INSUFFICIENT_PAYMENT (err u103))
(define-constant ERR_ALREADY_COMPLETED (err u104))
(define-constant ERR_CONSULTATION_EXPIRED (err u105))
(define-constant ERR_INVALID_PARTICIPANT (err u106))
(define-constant ERR_PAYMENT_FAILED (err u107))

(define-constant STATUS_PENDING u0)
(define-constant STATUS_CONFIRMED u1)
(define-constant STATUS_IN_PROGRESS u2)
(define-constant STATUS_COMPLETED u3)
(define-constant STATUS_CANCELLED u4)
(define-constant STATUS_DISPUTED u5)

(define-data-var consultation-counter uint u0)
(define-data-var platform-fee-percentage uint u250)

(define-map consultations
  uint
  {
    patient: principal,
    doctor: principal,
    amount: uint,
    status: uint,
    created-at: uint,
    scheduled-at: uint,
    completed-at: (optional uint),
    dispute-deadline: uint,
    consultation-type: (string-ascii 50),
    notes: (string-ascii 500)
  }
)

(define-map escrow-balances
  uint
  {
    total-amount: uint,
    platform-fee: uint,
    doctor-payment: uint,
    released: bool
  }
)

(define-map doctor-profiles
  principal
  {
    name: (string-ascii 100),
    specialty: (string-ascii 100),
    license-number: (string-ascii 50),
    verified: bool,
    total-consultations: uint,
    rating: uint
  }
)

(define-map patient-profiles
  principal
  {
    name: (string-ascii 100),
    total-consultations: uint,
    active-consultations: uint
  }
)

(define-map consultation-reviews
  uint
  {
    patient-rating: uint,
    doctor-rating: uint,
    patient-review: (string-ascii 500),
    doctor-review: (string-ascii 500)
  }
)

(define-public (register-doctor (name (string-ascii 100)) (specialty (string-ascii 100)) (license-number (string-ascii 50)))
  (begin
    (map-set doctor-profiles tx-sender {
      name: name,
      specialty: specialty,
      license-number: license-number,
      verified: false,
      total-consultations: u0,
      rating: u0
    })
    (ok true)
  )
)

(define-public (register-patient (name (string-ascii 100)))
  (begin
    (map-set patient-profiles tx-sender {
      name: name,
      total-consultations: u0,
      active-consultations: u0
    })
    (ok true)
  )
)

(define-public (verify-doctor (doctor principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (match (map-get? doctor-profiles doctor)
      profile (begin
        (map-set doctor-profiles doctor (merge profile { verified: true }))
        (ok true)
      )
      ERR_NOT_AUTHORIZED
    )
  )
)

(define-public (create-consultation 
  (doctor principal) 
  (amount uint) 
  (scheduled-at uint) 
  (consultation-type (string-ascii 50))
  (notes (string-ascii 500))
)
  (let
    (
      (consultation-id (+ (var-get consultation-counter) u1))
      (platform-fee (/ (* amount (var-get platform-fee-percentage)) u10000))
      (doctor-payment (- amount platform-fee))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    (asserts! (> amount u0) ERR_INSUFFICIENT_PAYMENT)
    (asserts! (> scheduled-at current-time) ERR_INVALID_STATUS)
    (asserts! (is-some (map-get? doctor-profiles doctor)) ERR_NOT_AUTHORIZED)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (map-set consultations consultation-id {
      patient: tx-sender,
      doctor: doctor,
      amount: amount,
      status: STATUS_PENDING,
      created-at: current-time,
      scheduled-at: scheduled-at,
      completed-at: none,
      dispute-deadline: (+ scheduled-at u144000),
      consultation-type: consultation-type,
      notes: notes
    })
    
    (map-set escrow-balances consultation-id {
      total-amount: amount,
      platform-fee: platform-fee,
      doctor-payment: doctor-payment,
      released: false
    })
    
    (var-set consultation-counter consultation-id)
    
    (match (map-get? patient-profiles tx-sender)
      profile (map-set patient-profiles tx-sender 
        (merge profile { active-consultations: (+ (get active-consultations profile) u1) }))
      true
    )
    
    (ok consultation-id)
  )
)

(define-public (confirm-consultation (consultation-id uint))
  (match (map-get? consultations consultation-id)
    consultation
    (begin
      (asserts! (is-eq tx-sender (get doctor consultation)) ERR_NOT_AUTHORIZED)
      (asserts! (is-eq (get status consultation) STATUS_PENDING) ERR_INVALID_STATUS)
      
      (map-set consultations consultation-id 
        (merge consultation { status: STATUS_CONFIRMED }))
      (ok true)
    )
    ERR_CONSULTATION_NOT_FOUND
  )
)

(define-public (start-consultation (consultation-id uint))
  (match (map-get? consultations consultation-id)
    consultation
    (begin
      (asserts! (is-eq tx-sender (get doctor consultation)) ERR_NOT_AUTHORIZED)
      (asserts! (is-eq (get status consultation) STATUS_CONFIRMED) ERR_INVALID_STATUS)
      
      (map-set consultations consultation-id 
        (merge consultation { status: STATUS_IN_PROGRESS }))
      (ok true)
    )
    ERR_CONSULTATION_NOT_FOUND
  )
)

(define-public (complete-consultation (consultation-id uint))
  (let
    (
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    (match (map-get? consultations consultation-id)
      consultation
      (begin
        (asserts! (is-eq tx-sender (get doctor consultation)) ERR_NOT_AUTHORIZED)
        (asserts! (is-eq (get status consultation) STATUS_IN_PROGRESS) ERR_INVALID_STATUS)
        
        (map-set consultations consultation-id 
          (merge consultation { 
            status: STATUS_COMPLETED,
            completed-at: (some current-time)
          }))
        (ok true)
      )
      ERR_CONSULTATION_NOT_FOUND
    )
  )
)

(define-public (release-payment (consultation-id uint))
  (let
    (
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    (match (map-get? consultations consultation-id)
      consultation
      (match (map-get? escrow-balances consultation-id)
        escrow
        (begin
          (asserts! (or 
            (is-eq tx-sender (get patient consultation))
            (and 
              (is-eq (get status consultation) STATUS_COMPLETED)
              (> current-time (+ (unwrap-panic (get completed-at consultation)) u14400))
            )
          ) ERR_NOT_AUTHORIZED)
          (asserts! (is-eq (get status consultation) STATUS_COMPLETED) ERR_INVALID_STATUS)
          (asserts! (not (get released escrow)) ERR_ALREADY_COMPLETED)
          
          (try! (as-contract (stx-transfer? (get doctor-payment escrow) tx-sender (get doctor consultation))))
          (try! (as-contract (stx-transfer? (get platform-fee escrow) tx-sender CONTRACT_OWNER)))
          
          (map-set escrow-balances consultation-id 
            (merge escrow { released: true }))
          
          (match (map-get? doctor-profiles (get doctor consultation))
            doctor-profile (map-set doctor-profiles (get doctor consultation)
              (merge doctor-profile { 
                total-consultations: (+ (get total-consultations doctor-profile) u1) 
              }))
            true
          )
          
          (match (map-get? patient-profiles (get patient consultation))
            patient-profile (map-set patient-profiles (get patient consultation)
              (merge patient-profile { 
                total-consultations: (+ (get total-consultations patient-profile) u1),
                active-consultations: (- (get active-consultations patient-profile) u1)
              }))
            true
          )
          
          (ok true)
        )
        ERR_CONSULTATION_NOT_FOUND
      )
      ERR_CONSULTATION_NOT_FOUND
    )
  )
)

(define-public (cancel-consultation (consultation-id uint))
  (match (map-get? consultations consultation-id)
    consultation
    (match (map-get? escrow-balances consultation-id)
      escrow
      (begin
        (asserts! (or 
          (is-eq tx-sender (get patient consultation))
          (is-eq tx-sender (get doctor consultation))
        ) ERR_NOT_AUTHORIZED)
        (asserts! (or 
          (is-eq (get status consultation) STATUS_PENDING)
          (is-eq (get status consultation) STATUS_CONFIRMED)
        ) ERR_INVALID_STATUS)
        (asserts! (not (get released escrow)) ERR_ALREADY_COMPLETED)
        
        (try! (as-contract (stx-transfer? (get total-amount escrow) tx-sender (get patient consultation))))
        
        (map-set consultations consultation-id 
          (merge consultation { status: STATUS_CANCELLED }))
        (map-set escrow-balances consultation-id 
          (merge escrow { released: true }))
        
        (match (map-get? patient-profiles (get patient consultation))
          patient-profile (map-set patient-profiles (get patient consultation)
            (merge patient-profile { 
              active-consultations: (- (get active-consultations patient-profile) u1)
            }))
          true
        )
        
        (ok true)
      )
      ERR_CONSULTATION_NOT_FOUND
    )
    ERR_CONSULTATION_NOT_FOUND
  )
)

(define-public (submit-review 
  (consultation-id uint) 
  (rating uint) 
  (review (string-ascii 500))
)
  (match (map-get? consultations consultation-id)
    consultation
    (begin
      (asserts! (or 
        (is-eq tx-sender (get patient consultation))
        (is-eq tx-sender (get doctor consultation))
      ) ERR_NOT_AUTHORIZED)
      (asserts! (is-eq (get status consultation) STATUS_COMPLETED) ERR_INVALID_STATUS)
      (asserts! (<= rating u5) ERR_INVALID_STATUS)
      
      (match (map-get? consultation-reviews consultation-id)
        existing-review
        (if (is-eq tx-sender (get patient consultation))
          (map-set consultation-reviews consultation-id 
            (merge existing-review { 
              patient-rating: rating,
              patient-review: review 
            }))
          (map-set consultation-reviews consultation-id 
            (merge existing-review { 
              doctor-rating: rating,
              doctor-review: review 
            }))
        )
        (if (is-eq tx-sender (get patient consultation))
          (map-set consultation-reviews consultation-id {
            patient-rating: rating,
            doctor-rating: u0,
            patient-review: review,
            doctor-review: ""
          })
          (map-set consultation-reviews consultation-id {
            patient-rating: u0,
            doctor-rating: rating,
            patient-review: "",
            doctor-review: review
          })
        )
      )
      (ok true)
    )
    ERR_CONSULTATION_NOT_FOUND
  )
)

(define-read-only (get-consultation (consultation-id uint))
  (map-get? consultations consultation-id)
)

(define-read-only (get-escrow-balance (consultation-id uint))
  (map-get? escrow-balances consultation-id)
)

(define-read-only (get-doctor-profile (doctor principal))
  (map-get? doctor-profiles doctor)
)

(define-read-only (get-patient-profile (patient principal))
  (map-get? patient-profiles patient)
)

(define-read-only (get-consultation-review (consultation-id uint))
  (map-get? consultation-reviews consultation-id)
)

(define-read-only (get-consultation-counter)
  (var-get consultation-counter)
)

(define-read-only (get-platform-fee-percentage)
  (var-get platform-fee-percentage)
)