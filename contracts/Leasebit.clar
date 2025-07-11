
;; title: Leasebit
;; version:
;; summary:
;; description:
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_AMOUNT (err u101))
(define-constant ERR_PROPERTY_NOT_FOUND (err u102))
(define-constant ERR_LEASE_NOT_FOUND (err u103))
(define-constant ERR_PAYMENT_OVERDUE (err u104))
(define-constant ERR_LEASE_COMPLETED (err u105))
(define-constant ERR_INSUFFICIENT_PAYMENT (err u106))
(define-constant ERR_LEASE_ACTIVE (err u107))
(define-constant ERR_NOT_TENANT (err u108))
(define-constant ERR_APPROVAL_NOT_FOUND (err u109))
(define-constant ERR_ALREADY_APPROVED (err u110))
(define-constant ERR_INSUFFICIENT_APPROVALS (err u111))
(define-constant ERR_APPROVAL_EXPIRED (err u112))
(define-constant ERR_INVALID_APPROVER (err u113))

(define-data-var next-property-id uint u1)
(define-data-var next-lease-id uint u1)
(define-data-var next-approval-id uint u1)

(define-map properties
  { property-id: uint }
  {
    owner: principal,
    total-price: uint,
    monthly-rent: uint,
    lease-duration-months: uint,
    is-available: bool,
    metadata: (string-ascii 256)
  }
)

(define-map leases
  { lease-id: uint }
  {
    property-id: uint,
    tenant: principal,
    landlord: principal,
    total-price: uint,
    monthly-rent: uint,
    lease-duration-months: uint,
    payments-made: uint,
    total-paid: uint,
    start-block: uint,
    last-payment-block: uint,
    is-active: bool,
    is-completed: bool
  }
)

(define-map tenant-leases
  { tenant: principal }
  { lease-ids: (list 10 uint) }
)

(define-map landlord-properties
  { landlord: principal }
  { property-ids: (list 20 uint) }
)

(define-map lease-approvals
  { approval-id: uint }
  {
    property-id: uint,
    tenant: principal,
    landlord: principal,
    required-approvers: (list 5 principal),
    approved-by: (list 5 principal),
    approvals-received: uint,
    approvals-required: uint,
    expiry-block: uint,
    is-approved: bool,
    is-expired: bool,
    created-at: uint
  }
)

(define-map property-approvers
  { property-id: uint }
  { approvers: (list 5 principal) }
)

(define-map approval-votes
  { approval-id: uint, approver: principal }
  { has-voted: bool, vote-block: uint }
)

(define-public (create-property (total-price uint) (monthly-rent uint) (lease-duration-months uint) (metadata (string-ascii 256)))
  (let
    (
      (property-id (var-get next-property-id))
      (current-properties (default-to { property-ids: (list) } (map-get? landlord-properties { landlord: tx-sender })))
    )
    (asserts! (> total-price u0) ERR_INVALID_AMOUNT)
    (asserts! (> monthly-rent u0) ERR_INVALID_AMOUNT)
    (asserts! (> lease-duration-months u0) ERR_INVALID_AMOUNT)
    
    (map-set properties
      { property-id: property-id }
      {
        owner: tx-sender,
        total-price: total-price,
        monthly-rent: monthly-rent,
        lease-duration-months: lease-duration-months,
        is-available: true,
        metadata: metadata
      }
    )
    
    (map-set landlord-properties
      { landlord: tx-sender }
      { property-ids: (unwrap! (as-max-len? (append (get property-ids current-properties) property-id) u20) ERR_INVALID_AMOUNT) }
    )
    
    (var-set next-property-id (+ property-id u1))
    (ok property-id)
  )
)

(define-public (start-lease (property-id uint))
  (let
    (
      (property (unwrap! (map-get? properties { property-id: property-id }) ERR_PROPERTY_NOT_FOUND))
      (lease-id (var-get next-lease-id))
      (current-tenant-leases (default-to { lease-ids: (list) } (map-get? tenant-leases { tenant: tx-sender })))
    )
    (asserts! (get is-available property) ERR_LEASE_ACTIVE)
    (asserts! (not (is-eq tx-sender (get owner property))) ERR_UNAUTHORIZED)
    
    (try! (stx-transfer? (get monthly-rent property) tx-sender (get owner property)))
    
    (map-set leases
      { lease-id: lease-id }
      {
        property-id: property-id,
        tenant: tx-sender,
        landlord: (get owner property),
        total-price: (get total-price property),
        monthly-rent: (get monthly-rent property),
        lease-duration-months: (get lease-duration-months property),
        payments-made: u1,
        total-paid: (get monthly-rent property),
        start-block: stacks-block-height,
        last-payment-block: stacks-block-height,
        is-active: true,
        is-completed: false
      }
    )
    
    (map-set properties
      { property-id: property-id }
      (merge property { is-available: false })
    )
    
    (map-set tenant-leases
      { tenant: tx-sender }
      { lease-ids: (unwrap! (as-max-len? (append (get lease-ids current-tenant-leases) lease-id) u10) ERR_INVALID_AMOUNT) }
    )
    
    (var-set next-lease-id (+ lease-id u1))
    (ok lease-id)
  )
)

(define-public (make-payment (lease-id uint))
  (let
    (
      (lease (unwrap! (map-get? leases { lease-id: lease-id }) ERR_LEASE_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get tenant lease)) ERR_NOT_TENANT)
    (asserts! (get is-active lease) ERR_LEASE_COMPLETED)
    (asserts! (< (get payments-made lease) (get lease-duration-months lease)) ERR_LEASE_COMPLETED)
    
    (try! (stx-transfer? (get monthly-rent lease) tx-sender (get landlord lease)))
    
    (let
      (
        (new-payments-made (+ (get payments-made lease) u1))
        (new-total-paid (+ (get total-paid lease) (get monthly-rent lease)))
        (is-lease-completed (>= new-payments-made (get lease-duration-months lease)))
      )
      (map-set leases
        { lease-id: lease-id }
        (merge lease {
          payments-made: new-payments-made,
          total-paid: new-total-paid,
          last-payment-block: stacks-block-height,
          is-completed: is-lease-completed,
          is-active: (not is-lease-completed)
        })
      )
      
      (if is-lease-completed
        (begin
          (map-set properties
            { property-id: (get property-id lease) }
            (merge (unwrap-panic (map-get? properties { property-id: (get property-id lease) })) { owner: (get tenant lease), is-available: true })
          )
          (ok { payment-made: true, lease-completed: true, ownership-transferred: true })
        )
        (ok { payment-made: true, lease-completed: false, ownership-transferred: false })
      )
    )
  )
)

(define-public (cancel-lease (lease-id uint))
  (let
    (
      (lease (unwrap! (map-get? leases { lease-id: lease-id }) ERR_LEASE_NOT_FOUND))
    )
    (asserts! (or (is-eq tx-sender (get tenant lease)) (is-eq tx-sender (get landlord lease))) ERR_UNAUTHORIZED)
    (asserts! (get is-active lease) ERR_LEASE_COMPLETED)
    
    (map-set leases
      { lease-id: lease-id }
      (merge lease { is-active: false })
    )
    
    (map-set properties
      { property-id: (get property-id lease) }
      (merge (unwrap-panic (map-get? properties { property-id: (get property-id lease) })) { is-available: true })
    )
    
    (ok true)
  )
)

(define-read-only (get-property (property-id uint))
  (map-get? properties { property-id: property-id })
)

(define-read-only (get-lease (lease-id uint))
  (map-get? leases { lease-id: lease-id })
)

(define-read-only (get-tenant-leases (tenant principal))
  (map-get? tenant-leases { tenant: tenant })
)

(define-read-only (get-landlord-properties (landlord principal))
  (map-get? landlord-properties { landlord: landlord })
)

(define-read-only (get-lease-progress (lease-id uint))
  (match (map-get? leases { lease-id: lease-id })
    lease (ok {
      payments-made: (get payments-made lease),
      total-payments-required: (get lease-duration-months lease),
      total-paid: (get total-paid lease),
      remaining-amount: (- (get total-price lease) (get total-paid lease)),
      completion-percentage: (/ (* (get payments-made lease) u100) (get lease-duration-months lease))
    })
    ERR_LEASE_NOT_FOUND
  )
)

(define-read-only (is-payment-overdue (lease-id uint) (blocks-per-month uint))
  (match (map-get? leases { lease-id: lease-id })
    lease 
      (if (get is-active lease)
        (ok (> (- stacks-block-height (get last-payment-block lease)) blocks-per-month))
        (ok false)
      )
    ERR_LEASE_NOT_FOUND
  )
)

(define-read-only (get-next-property-id)
  (var-get next-property-id)
)

(define-read-only (get-next-lease-id)
  (var-get next-lease-id)
)

(define-public (set-property-approvers (property-id uint) (approvers (list 5 principal)))
  (let
    (
      (property (unwrap! (map-get? properties { property-id: property-id }) ERR_PROPERTY_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get owner property)) ERR_UNAUTHORIZED)
    (asserts! (> (len approvers) u0) ERR_INVALID_APPROVER)
    
    (map-set property-approvers
      { property-id: property-id }
      { approvers: approvers }
    )
    
    (ok true)
  )
)

(define-public (request-lease-approval (property-id uint) (expiry-blocks uint))
  (let
    (
      (property (unwrap! (map-get? properties { property-id: property-id }) ERR_PROPERTY_NOT_FOUND))
      (approval-id (var-get next-approval-id))
      (approvers-data (map-get? property-approvers { property-id: property-id }))
      (required-approvers (match approvers-data 
        some-approvers (get approvers some-approvers)
        (list (get owner property))
      ))
      (approvals-required (len required-approvers))
    )
    (asserts! (get is-available property) ERR_LEASE_ACTIVE)
    (asserts! (not (is-eq tx-sender (get owner property))) ERR_UNAUTHORIZED)
    (asserts! (> expiry-blocks u0) ERR_INVALID_AMOUNT)
    (asserts! (> approvals-required u0) ERR_INVALID_APPROVER)
    
    (map-set lease-approvals
      { approval-id: approval-id }
      {
        property-id: property-id,
        tenant: tx-sender,
        landlord: (get owner property),
        required-approvers: required-approvers,
        approved-by: (list),
        approvals-received: u0,
        approvals-required: approvals-required,
        expiry-block: (+ stacks-block-height expiry-blocks),
        is-approved: false,
        is-expired: false,
        created-at: stacks-block-height
      }
    )
    
    (var-set next-approval-id (+ approval-id u1))
    (ok approval-id)
  )
)

(define-public (approve-lease (approval-id uint))
  (let
    (
      (approval (unwrap! (map-get? lease-approvals { approval-id: approval-id }) ERR_APPROVAL_NOT_FOUND))
      (existing-vote (map-get? approval-votes { approval-id: approval-id, approver: tx-sender }))
    )
    (asserts! (< stacks-block-height (get expiry-block approval)) ERR_APPROVAL_EXPIRED)
    (asserts! (not (get is-expired approval)) ERR_APPROVAL_EXPIRED)
    (asserts! (not (get is-approved approval)) ERR_ALREADY_APPROVED)
    (asserts! (is-some (index-of (get required-approvers approval) tx-sender)) ERR_INVALID_APPROVER)
    (asserts! (is-none existing-vote) ERR_ALREADY_APPROVED)
    
    (map-set approval-votes
      { approval-id: approval-id, approver: tx-sender }
      { has-voted: true, vote-block: stacks-block-height }
    )
    
    (let
      (
        (new-approved-by (unwrap! (as-max-len? (append (get approved-by approval) tx-sender) u5) ERR_INVALID_APPROVER))
        (new-approvals-received (+ (get approvals-received approval) u1))
        (is-fully-approved (>= new-approvals-received (get approvals-required approval)))
      )
      (map-set lease-approvals
        { approval-id: approval-id }
        (merge approval {
          approved-by: new-approved-by,
          approvals-received: new-approvals-received,
          is-approved: is-fully-approved
        })
      )
      
      (ok { approved: true, fully-approved: is-fully-approved })
    )
  )
)

(define-public (start-lease-with-approval (approval-id uint))
  (let
    (
      (approval (unwrap! (map-get? lease-approvals { approval-id: approval-id }) ERR_APPROVAL_NOT_FOUND))
      (property (unwrap! (map-get? properties { property-id: (get property-id approval) }) ERR_PROPERTY_NOT_FOUND))
      (lease-id (var-get next-lease-id))
      (current-tenant-leases (default-to { lease-ids: (list) } (map-get? tenant-leases { tenant: tx-sender })))
    )
    (asserts! (is-eq tx-sender (get tenant approval)) ERR_UNAUTHORIZED)
    (asserts! (get is-approved approval) ERR_INSUFFICIENT_APPROVALS)
    (asserts! (< stacks-block-height (get expiry-block approval)) ERR_APPROVAL_EXPIRED)
    (asserts! (not (get is-expired approval)) ERR_APPROVAL_EXPIRED)
    (asserts! (get is-available property) ERR_LEASE_ACTIVE)
    
    (try! (stx-transfer? (get monthly-rent property) tx-sender (get owner property)))
    
    (map-set leases
      { lease-id: lease-id }
      {
        property-id: (get property-id approval),
        tenant: tx-sender,
        landlord: (get owner property),
        total-price: (get total-price property),
        monthly-rent: (get monthly-rent property),
        lease-duration-months: (get lease-duration-months property),
        payments-made: u1,
        total-paid: (get monthly-rent property),
        start-block: stacks-block-height,
        last-payment-block: stacks-block-height,
        is-active: true,
        is-completed: false
      }
    )
    
    (map-set properties
      { property-id: (get property-id approval) }
      (merge property { is-available: false })
    )
    
    (map-set tenant-leases
      { tenant: tx-sender }
      { lease-ids: (unwrap! (as-max-len? (append (get lease-ids current-tenant-leases) lease-id) u10) ERR_INVALID_AMOUNT) }
    )
    
    (map-set lease-approvals
      { approval-id: approval-id }
      (merge approval { is-expired: true })
    )
    
    (var-set next-lease-id (+ lease-id u1))
    (ok lease-id)
  )
)

(define-public (expire-approval (approval-id uint))
  (let
    (
      (approval (unwrap! (map-get? lease-approvals { approval-id: approval-id }) ERR_APPROVAL_NOT_FOUND))
    )
    (asserts! (>= stacks-block-height (get expiry-block approval)) ERR_APPROVAL_EXPIRED)
    (asserts! (not (get is-expired approval)) ERR_ALREADY_APPROVED)
    
    (map-set lease-approvals
      { approval-id: approval-id }
      (merge approval { is-expired: true })
    )
    
    (ok true)
  )
)

(define-read-only (get-lease-approval (approval-id uint))
  (map-get? lease-approvals { approval-id: approval-id })
)

(define-read-only (get-property-approvers (property-id uint))
  (map-get? property-approvers { property-id: property-id })
)

(define-read-only (get-approval-vote (approval-id uint) (approver principal))
  (map-get? approval-votes { approval-id: approval-id, approver: approver })
)

(define-read-only (get-approval-status (approval-id uint))
  (match (map-get? lease-approvals { approval-id: approval-id })
    approval (ok {
      is-approved: (get is-approved approval),
      is-expired: (get is-expired approval),
      approvals-received: (get approvals-received approval),
      approvals-required: (get approvals-required approval),
      expiry-block: (get expiry-block approval),
      blocks-remaining: (if (>= stacks-block-height (get expiry-block approval)) u0 (- (get expiry-block approval) stacks-block-height))
    })
    ERR_APPROVAL_NOT_FOUND
  )
)

(define-read-only (get-next-approval-id)
  (var-get next-approval-id)
)