(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_CONSULTATION_NOT_FOUND (err u101))
(define-constant ERR_INVALID_STATUS (err u102))
(define-constant ERR_INSUFFICIENT_PAYMENT (err u103))
(define-constant ERR_ALREADY_COMPLETED (err u104))
(define-constant ERR_CONSULTATION_EXPIRED (err u105))
(define-constant ERR_INVALID_PARTICIPANT (err u106))
(define-constant ERR_PAYMENT_FAILED (err u107))
(define-constant ERR_SUBSCRIPTION_NOT_FOUND (err u108))
(define-constant ERR_SUBSCRIPTION_INACTIVE (err u109))
(define-constant ERR_SUBSCRIPTION_EXPIRED (err u110))
(define-constant ERR_INSUFFICIENT_CONSULTATIONS (err u111))
(define-constant ERR_SUBSCRIPTION_ALREADY_ACTIVE (err u112))
(define-constant ERR_MEDICAL_RECORD_NOT_FOUND (err u113))
(define-constant ERR_ACCESS_DENIED (err u114))
(define-constant ERR_RECORD_ALREADY_EXISTS (err u115))
(define-constant ERR_INVALID_RECORD_TYPE (err u116))
(define-constant ERR_PERMISSION_NOT_FOUND (err u117))

(define-constant STATUS_PENDING u0)
(define-constant STATUS_CONFIRMED u1)
(define-constant STATUS_IN_PROGRESS u2)
(define-constant STATUS_COMPLETED u3)
(define-constant STATUS_CANCELLED u4)
(define-constant STATUS_DISPUTED u5)

(define-constant SUBSCRIPTION_ACTIVE u0)
(define-constant SUBSCRIPTION_PAUSED u1)
(define-constant SUBSCRIPTION_CANCELLED u2)
(define-constant SUBSCRIPTION_EXPIRED u3)

;; Medical record access levels
(define-constant ACCESS_READ u0)
(define-constant ACCESS_WRITE u1)
(define-constant ACCESS_FULL u2)

;; Medical record types
(define-constant RECORD_GENERAL u0)
(define-constant RECORD_DIAGNOSIS u1) 
(define-constant RECORD_PRESCRIPTION u2)
(define-constant RECORD_LAB_RESULT u3)
(define-constant RECORD_IMAGING u4)

(define-data-var consultation-counter uint u0)
(define-data-var medical-record-counter uint u0)
(define-data-var subscription-counter uint u0)
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

(define-map subscription-plans
  uint
  {
    doctor: principal,
    plan-name: (string-ascii 100),
    description: (string-ascii 500),
    monthly-fee: uint,
    consultations-per-month: uint,
    consultation-duration: uint,
    specialty-focus: (string-ascii 100),
    active: bool,
    created-at: uint
  }
)

(define-map patient-subscriptions
  uint
  {
    patient: principal,
    plan-id: uint,
    status: uint,
    start-date: uint,
    end-date: uint,
    consultations-used: uint,
    consultations-remaining: uint,
    last-payment: uint,
    auto-renew: bool
  }
)

(define-map subscription-consultations
  uint
  {
    subscription-id: uint,
    consultation-id: uint,
    scheduled-at: uint,
    status: uint,
    completed-at: (optional uint)
  }
)

;; Medical records storage
(define-map medical-records
  uint
  {
    patient: principal,
    record-type: uint,
    title: (string-ascii 100),
    content: (string-ascii 1000),
    created-by: principal,
    created-at: uint,
    updated-at: uint,
    consultation-id: (optional uint),
    is-sensitive: bool
  }
)

;; Access permissions for medical records
(define-map record-permissions
  {record-id: uint, healthcare-provider: principal}
  {
    access-level: uint,
    granted-at: uint,
    expires-at: (optional uint),
    granted-by: principal
  }
)

;; Emergency access permissions
(define-map emergency-contacts
  principal
  {
    emergency-contact: principal,
    relationship: (string-ascii 50),
    access-level: uint,
    active: bool
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

(define-public (create-subscription-plan
  (plan-name (string-ascii 100))
  (description (string-ascii 500))
  (monthly-fee uint)
  (consultations-per-month uint)
  (consultation-duration uint)
  (specialty-focus (string-ascii 100))
)
  (let
    (
      (plan-id (+ (var-get subscription-counter) u1))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    (asserts! (is-some (map-get? doctor-profiles tx-sender)) ERR_NOT_AUTHORIZED)
    (asserts! (> monthly-fee u0) ERR_INSUFFICIENT_PAYMENT)
    (asserts! (> consultations-per-month u0) ERR_INVALID_STATUS)
    (asserts! (> consultation-duration u0) ERR_INVALID_STATUS)
    
    (map-set subscription-plans plan-id {
      doctor: tx-sender,
      plan-name: plan-name,
      description: description,
      monthly-fee: monthly-fee,
      consultations-per-month: consultations-per-month,
      consultation-duration: consultation-duration,
      specialty-focus: specialty-focus,
      active: true,
      created-at: current-time
    })
    
    (var-set subscription-counter plan-id)
    (ok plan-id)
  )
)

(define-public (subscribe-to-plan (plan-id uint))
  (let
    (
      (subscription-id (+ (var-get subscription-counter) u1))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
      (end-time (+ current-time u2592000))
    )
    (match (map-get? subscription-plans plan-id)
      plan
      (begin
        (asserts! (is-some (map-get? patient-profiles tx-sender)) ERR_NOT_AUTHORIZED)
        (asserts! (get active plan) ERR_SUBSCRIPTION_INACTIVE)
        
        (try! (stx-transfer? (get monthly-fee plan) tx-sender (as-contract tx-sender)))
        
        (map-set patient-subscriptions subscription-id {
          patient: tx-sender,
          plan-id: plan-id,
          status: SUBSCRIPTION_ACTIVE,
          start-date: current-time,
          end-date: end-time,
          consultations-used: u0,
          consultations-remaining: (get consultations-per-month plan),
          last-payment: current-time,
          auto-renew: false
        })
        
        (var-set subscription-counter subscription-id)
        (ok subscription-id)
      )
      ERR_SUBSCRIPTION_NOT_FOUND
    )
  )
)

(define-public (renew-subscription (subscription-id uint))
  (let
    (
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
      (new-end-time (+ current-time u2592000))
    )
    (match (map-get? patient-subscriptions subscription-id)
      subscription
      (match (map-get? subscription-plans (get plan-id subscription))
        plan
        (begin
          (asserts! (is-eq tx-sender (get patient subscription)) ERR_NOT_AUTHORIZED)
          (asserts! (get active plan) ERR_SUBSCRIPTION_INACTIVE)
          
          (try! (stx-transfer? (get monthly-fee plan) tx-sender (as-contract tx-sender)))
          
          (map-set patient-subscriptions subscription-id 
            (merge subscription {
              status: SUBSCRIPTION_ACTIVE,
              end-date: new-end-time,
              consultations-used: u0,
              consultations-remaining: (get consultations-per-month plan),
              last-payment: current-time
            }))
          (ok true)
        )
        ERR_SUBSCRIPTION_NOT_FOUND
      )
      ERR_SUBSCRIPTION_NOT_FOUND
    )
  )
)

(define-public (cancel-subscription (subscription-id uint))
  (match (map-get? patient-subscriptions subscription-id)
    subscription
    (begin
      (asserts! (is-eq tx-sender (get patient subscription)) ERR_NOT_AUTHORIZED)
      (asserts! (is-eq (get status subscription) SUBSCRIPTION_ACTIVE) ERR_SUBSCRIPTION_INACTIVE)
      
      (map-set patient-subscriptions subscription-id 
        (merge subscription { status: SUBSCRIPTION_CANCELLED }))
      (ok true)
    )
    ERR_SUBSCRIPTION_NOT_FOUND
  )
)

(define-public (pause-subscription (subscription-id uint))
  (match (map-get? patient-subscriptions subscription-id)
    subscription
    (begin
      (asserts! (is-eq tx-sender (get patient subscription)) ERR_NOT_AUTHORIZED)
      (asserts! (is-eq (get status subscription) SUBSCRIPTION_ACTIVE) ERR_SUBSCRIPTION_INACTIVE)
      
      (map-set patient-subscriptions subscription-id 
        (merge subscription { status: SUBSCRIPTION_PAUSED }))
      (ok true)
    )
    ERR_SUBSCRIPTION_NOT_FOUND
  )
)

(define-public (resume-subscription (subscription-id uint))
  (let
    (
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    (match (map-get? patient-subscriptions subscription-id)
      subscription
      (begin
        (asserts! (is-eq tx-sender (get patient subscription)) ERR_NOT_AUTHORIZED)
        (asserts! (is-eq (get status subscription) SUBSCRIPTION_PAUSED) ERR_SUBSCRIPTION_INACTIVE)
        (asserts! (< current-time (get end-date subscription)) ERR_SUBSCRIPTION_EXPIRED)
        
        (map-set patient-subscriptions subscription-id 
          (merge subscription { status: SUBSCRIPTION_ACTIVE }))
        (ok true)
      )
      ERR_SUBSCRIPTION_NOT_FOUND
    )
  )
)

(define-public (book-subscription-consultation 
  (subscription-id uint)
  (scheduled-at uint)
  (consultation-type (string-ascii 50))
  (notes (string-ascii 500))
)
  (let
    (
      (consultation-id (+ (var-get consultation-counter) u1))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    (match (map-get? patient-subscriptions subscription-id)
      subscription
      (match (map-get? subscription-plans (get plan-id subscription))
        plan
        (begin
          (asserts! (is-eq tx-sender (get patient subscription)) ERR_NOT_AUTHORIZED)
          (asserts! (is-eq (get status subscription) SUBSCRIPTION_ACTIVE) ERR_SUBSCRIPTION_INACTIVE)
          (asserts! (< current-time (get end-date subscription)) ERR_SUBSCRIPTION_EXPIRED)
          (asserts! (> (get consultations-remaining subscription) u0) ERR_INSUFFICIENT_CONSULTATIONS)
          (asserts! (> scheduled-at current-time) ERR_INVALID_STATUS)
          
          (map-set consultations consultation-id {
            patient: tx-sender,
            doctor: (get doctor plan),
            amount: u0,
            status: STATUS_PENDING,
            created-at: current-time,
            scheduled-at: scheduled-at,
            completed-at: none,
            dispute-deadline: (+ scheduled-at u144000),
            consultation-type: consultation-type,
            notes: notes
          })
          
          (map-set subscription-consultations consultation-id {
            subscription-id: subscription-id,
            consultation-id: consultation-id,
            scheduled-at: scheduled-at,
            status: STATUS_PENDING,
            completed-at: none
          })
          
          (map-set patient-subscriptions subscription-id 
            (merge subscription {
              consultations-used: (+ (get consultations-used subscription) u1),
              consultations-remaining: (- (get consultations-remaining subscription) u1)
            }))
          
          (var-set consultation-counter consultation-id)
          (ok consultation-id)
        )
        ERR_SUBSCRIPTION_NOT_FOUND
      )
      ERR_SUBSCRIPTION_NOT_FOUND
    )
  )
)

(define-public (deactivate-subscription-plan (plan-id uint))
  (match (map-get? subscription-plans plan-id)
    plan
    (begin
      (asserts! (is-eq tx-sender (get doctor plan)) ERR_NOT_AUTHORIZED)
      (map-set subscription-plans plan-id 
        (merge plan { active: false }))
      (ok true)
    )
    ERR_SUBSCRIPTION_NOT_FOUND
  )
)

(define-public (update-subscription-plan 
  (plan-id uint)
  (monthly-fee uint)
  (consultations-per-month uint)
  (consultation-duration uint)
)
  (match (map-get? subscription-plans plan-id)
    plan
    (begin
      (asserts! (is-eq tx-sender (get doctor plan)) ERR_NOT_AUTHORIZED)
      (asserts! (> monthly-fee u0) ERR_INSUFFICIENT_PAYMENT)
      (asserts! (> consultations-per-month u0) ERR_INVALID_STATUS)
      (asserts! (> consultation-duration u0) ERR_INVALID_STATUS)
      
      (map-set subscription-plans plan-id 
        (merge plan {
          monthly-fee: monthly-fee,
          consultations-per-month: consultations-per-month,
          consultation-duration: consultation-duration
        }))
      (ok true)
    )
    ERR_SUBSCRIPTION_NOT_FOUND
  )
)

(define-read-only (get-subscription-plan (plan-id uint))
  (map-get? subscription-plans plan-id)
)

(define-read-only (get-patient-subscription (subscription-id uint))
  (map-get? patient-subscriptions subscription-id)
)

(define-read-only (get-subscription-consultation (consultation-id uint))
  (map-get? subscription-consultations consultation-id)
)

(define-read-only (get-subscription-counter)
  (var-get subscription-counter)
)

;; Create a new medical record
(define-public (create-medical-record
  (patient principal)
  (record-type uint)
  (title (string-ascii 100))
  (content (string-ascii 1000))
  (consultation-id (optional uint))
  (is-sensitive bool)
)
  (let
    (
      (record-id (+ (var-get medical-record-counter) u1))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    ;; Verify caller is authorized healthcare provider
    (asserts! (is-some (map-get? doctor-profiles tx-sender)) ERR_NOT_AUTHORIZED)
    ;; Verify record type is valid
    (asserts! (<= record-type RECORD_IMAGING) ERR_INVALID_RECORD_TYPE)
    
    (map-set medical-records record-id {
      patient: patient,
      record-type: record-type,
      title: title,
      content: content,
      created-by: tx-sender,
      created-at: current-time,
      updated-at: current-time,
      consultation-id: consultation-id,
      is-sensitive: is-sensitive
    })
    
    (var-set medical-record-counter record-id)
    (ok record-id)
  )
)

;; Update existing medical record
(define-public (update-medical-record
  (record-id uint)
  (content (string-ascii 1000))
)
  (let
    (
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    (match (map-get? medical-records record-id)
      record
      (begin
        ;; Verify caller is the original creator or has write access
        (asserts! (or 
          (is-eq tx-sender (get created-by record))
          (is-eq (get access-level (default-to {access-level: u0, granted-at: u0, expires-at: none, granted-by: (get patient record)} 
            (map-get? record-permissions {record-id: record-id, healthcare-provider: tx-sender}))) ACCESS_WRITE)
          (is-eq (get access-level (default-to {access-level: u0, granted-at: u0, expires-at: none, granted-by: (get patient record)} 
            (map-get? record-permissions {record-id: record-id, healthcare-provider: tx-sender}))) ACCESS_FULL)
        ) ERR_ACCESS_DENIED)
        
        (map-set medical-records record-id 
          (merge record {
            content: content,
            updated-at: current-time
          }))
        (ok true)
      )
      ERR_MEDICAL_RECORD_NOT_FOUND
    )
  )
)

;; Grant access to medical record
(define-public (grant-record-access
  (record-id uint)
  (healthcare-provider principal)
  (access-level uint)
  (expires-at (optional uint))
)
  (let
    (
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    (match (map-get? medical-records record-id)
      record
      (begin
        ;; Only patient or record creator can grant access
        (asserts! (or 
          (is-eq tx-sender (get patient record))
          (is-eq tx-sender (get created-by record))
        ) ERR_NOT_AUTHORIZED)
        ;; Verify access level is valid
        (asserts! (<= access-level ACCESS_FULL) ERR_INVALID_STATUS)
        ;; Verify healthcare provider exists
        (asserts! (is-some (map-get? doctor-profiles healthcare-provider)) ERR_NOT_AUTHORIZED)
        
        (map-set record-permissions 
          {record-id: record-id, healthcare-provider: healthcare-provider}
          {
            access-level: access-level,
            granted-at: current-time,
            expires-at: expires-at,
            granted-by: tx-sender
          })
        (ok true)
      )
      ERR_MEDICAL_RECORD_NOT_FOUND
    )
  )
)

;; Revoke access to medical record
(define-public (revoke-record-access
  (record-id uint)
  (healthcare-provider principal)
)
  (match (map-get? medical-records record-id)
    record
    (begin
      ;; Only patient or record creator can revoke access
      (asserts! (or 
        (is-eq tx-sender (get patient record))
        (is-eq tx-sender (get created-by record))
      ) ERR_NOT_AUTHORIZED)
      
      (map-delete record-permissions {record-id: record-id, healthcare-provider: healthcare-provider})
      (ok true)
    )
    ERR_MEDICAL_RECORD_NOT_FOUND
  )
)

;; Set emergency contact
(define-public (set-emergency-contact
  (emergency-contact principal)
  (relationship (string-ascii 50))
  (access-level uint)
)
  (begin
    ;; Verify caller is a patient
    (asserts! (is-some (map-get? patient-profiles tx-sender)) ERR_NOT_AUTHORIZED)
    ;; Verify access level is valid
    (asserts! (<= access-level ACCESS_FULL) ERR_INVALID_STATUS)
    
    (map-set emergency-contacts tx-sender {
      emergency-contact: emergency-contact,
      relationship: relationship,
      access-level: access-level,
      active: true
    })
    (ok true)
  )
)

;; Get medical record with access control
(define-public (get-medical-record-with-access (record-id uint))
  (let
    (
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    (match (map-get? medical-records record-id)
      record
      (begin
        ;; Check if caller has access
        (asserts! (or
          ;; Patient owns the record
          (is-eq tx-sender (get patient record))
          ;; Healthcare provider created the record
          (is-eq tx-sender (get created-by record))
          ;; Healthcare provider has been granted access
          (is-some (map-get? record-permissions {record-id: record-id, healthcare-provider: tx-sender}))
          ;; Emergency contact access
          (match (map-get? emergency-contacts (get patient record))
            emergency
            (and (is-eq tx-sender (get emergency-contact emergency)) (get active emergency))
            false
          )
        ) ERR_ACCESS_DENIED)
        
        ;; Check if access has expired
        (match (map-get? record-permissions {record-id: record-id, healthcare-provider: tx-sender})
          permission
          (match (get expires-at permission)
            expiry
            (asserts! (< current-time expiry) ERR_ACCESS_DENIED)
            true
          )
          true
        )
        
        (ok record)
      )
      ERR_MEDICAL_RECORD_NOT_FOUND
    )
  )
)

;; Read-only function to check access permissions
(define-read-only (has-record-access (record-id uint) (healthcare-provider principal))
  (let
    (
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    (match (map-get? medical-records record-id)
      record
      (or
        ;; Patient owns the record
        (is-eq healthcare-provider (get patient record))
        ;; Healthcare provider created the record
        (is-eq healthcare-provider (get created-by record))
        ;; Healthcare provider has valid permission
        (match (map-get? record-permissions {record-id: record-id, healthcare-provider: healthcare-provider})
          permission
          (match (get expires-at permission)
            expiry
            (< current-time expiry)
            true
          )
          false
        )
        ;; Emergency contact access
        (match (map-get? emergency-contacts (get patient record))
          emergency
          (and (is-eq healthcare-provider (get emergency-contact emergency)) (get active emergency))
          false
        )
      )
      false
    )
  )
)

;; Read-only functions for medical records
(define-read-only (get-medical-record (record-id uint))
  (map-get? medical-records record-id)
)

(define-read-only (get-record-permission (record-id uint) (healthcare-provider principal))
  (map-get? record-permissions {record-id: record-id, healthcare-provider: healthcare-provider})
)

(define-read-only (get-emergency-contact (patient principal))
  (map-get? emergency-contacts patient)
)

(define-read-only (get-medical-record-counter)
  (var-get medical-record-counter)
)



