
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
(define-constant ERR_MAINTENANCE_NOT_FOUND (err u114))
(define-constant ERR_INVALID_STATUS (err u115))
(define-constant ERR_MAINTENANCE_COMPLETED (err u116))
(define-constant ERR_INVALID_COST (err u117))

(define-data-var next-property-id uint u1)
(define-data-var next-lease-id uint u1)
(define-data-var next-approval-id uint u1)
(define-data-var next-maintenance-id uint u1)

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

(define-map maintenance-requests
  { maintenance-id: uint }
  {
    property-id: uint,
    lease-id: uint,
    requester: principal,
    landlord: principal,
    tenant: principal,
    category: (string-ascii 20),
    description: (string-ascii 500),
    estimated-cost: uint,
    actual-cost: uint,
    responsible-party: (string-ascii 10),
    status: (string-ascii 15),
    created-at: uint,
    approved-at: uint,
    completed-at: uint,
    cost-allocation: (string-ascii 15)
  }
)

(define-map property-maintenance-history
  { property-id: uint }
  { maintenance-ids: (list 50 uint), total-maintenance-cost: uint }
)

(define-map lease-maintenance-adjustments
  { lease-id: uint }
  { 
    total-tenant-costs: uint,
    total-landlord-costs: uint,
    rent-adjustments: uint,
    price-adjustments: uint
  }
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

(define-public (submit-maintenance-request (lease-id uint) (category (string-ascii 20)) (description (string-ascii 500)) (estimated-cost uint))
  (let
    (
      (lease (unwrap! (map-get? leases { lease-id: lease-id }) ERR_LEASE_NOT_FOUND))
      (maintenance-id (var-get next-maintenance-id))
      (property-history (default-to { maintenance-ids: (list), total-maintenance-cost: u0 } 
        (map-get? property-maintenance-history { property-id: (get property-id lease) })))
    )
    (asserts! (get is-active lease) ERR_LEASE_COMPLETED)
    (asserts! (or (is-eq tx-sender (get tenant lease)) (is-eq tx-sender (get landlord lease))) ERR_UNAUTHORIZED)
    (asserts! (> estimated-cost u0) ERR_INVALID_COST)
    
    (map-set maintenance-requests
      { maintenance-id: maintenance-id }
      {
        property-id: (get property-id lease),
        lease-id: lease-id,
        requester: tx-sender,
        landlord: (get landlord lease),
        tenant: (get tenant lease),
        category: category,
        description: description,
        estimated-cost: estimated-cost,
        actual-cost: u0,
        responsible-party: "pending",
        status: "submitted",
        created-at: stacks-block-height,
        approved-at: u0,
        completed-at: u0,
        cost-allocation: "pending"
      }
    )
    
    (map-set property-maintenance-history
      { property-id: (get property-id lease) }
      { 
        maintenance-ids: (unwrap! (as-max-len? (append (get maintenance-ids property-history) maintenance-id) u50) ERR_INVALID_AMOUNT),
        total-maintenance-cost: (get total-maintenance-cost property-history)
      }
    )
    
    (var-set next-maintenance-id (+ maintenance-id u1))
    (ok maintenance-id)
  )
)

(define-public (approve-maintenance-request (maintenance-id uint) (responsible-party (string-ascii 10)) (cost-allocation (string-ascii 15)))
  (let
    (
      (request (unwrap! (map-get? maintenance-requests { maintenance-id: maintenance-id }) ERR_MAINTENANCE_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get landlord request)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status request) "submitted") ERR_INVALID_STATUS)
    
    (map-set maintenance-requests
      { maintenance-id: maintenance-id }
      (merge request {
        responsible-party: responsible-party,
        status: "approved",
        approved-at: stacks-block-height,
        cost-allocation: cost-allocation
      })
    )
    
    (ok true)
  )
)

(define-public (complete-maintenance-request (maintenance-id uint) (actual-cost uint))
  (let
    (
      (request (unwrap! (map-get? maintenance-requests { maintenance-id: maintenance-id }) ERR_MAINTENANCE_NOT_FOUND))
      (lease-adjustments (default-to { total-tenant-costs: u0, total-landlord-costs: u0, rent-adjustments: u0, price-adjustments: u0 }
        (map-get? lease-maintenance-adjustments { lease-id: (get lease-id request) })))
      (property-history (unwrap! (map-get? property-maintenance-history { property-id: (get property-id request) }) ERR_PROPERTY_NOT_FOUND))
    )
    (asserts! (or (is-eq tx-sender (get landlord request)) (is-eq tx-sender (get tenant request))) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status request) "approved") ERR_INVALID_STATUS)
    (asserts! (> actual-cost u0) ERR_INVALID_COST)
    
    (map-set maintenance-requests
      { maintenance-id: maintenance-id }
      (merge request {
        actual-cost: actual-cost,
        status: "completed",
        completed-at: stacks-block-height
      })
    )
    
    (map-set property-maintenance-history
      { property-id: (get property-id request) }
      (merge property-history {
        total-maintenance-cost: (+ (get total-maintenance-cost property-history) actual-cost)
      })
    )
    
    (let
      (
        (tenant-cost-increase (if (is-eq (get responsible-party request) "tenant") actual-cost u0))
        (landlord-cost-increase (if (is-eq (get responsible-party request) "landlord") actual-cost u0))
        (rent-adjustment (if (is-eq (get cost-allocation request) "rent-increase") actual-cost u0))
        (price-adjustment (if (is-eq (get cost-allocation request) "price-increase") actual-cost u0))
      )
      (map-set lease-maintenance-adjustments
        { lease-id: (get lease-id request) }
        {
          total-tenant-costs: (+ (get total-tenant-costs lease-adjustments) tenant-cost-increase),
          total-landlord-costs: (+ (get total-landlord-costs lease-adjustments) landlord-cost-increase),
          rent-adjustments: (+ (get rent-adjustments lease-adjustments) rent-adjustment),
          price-adjustments: (+ (get price-adjustments lease-adjustments) price-adjustment)
        }
      )
    )
    
    (ok true)
  )
)

(define-public (reject-maintenance-request (maintenance-id uint) (reason (string-ascii 256)))
  (let
    (
      (request (unwrap! (map-get? maintenance-requests { maintenance-id: maintenance-id }) ERR_MAINTENANCE_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get landlord request)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status request) "submitted") ERR_INVALID_STATUS)
    
    (map-set maintenance-requests
      { maintenance-id: maintenance-id }
      (merge request {
        status: "rejected",
        approved-at: stacks-block-height
      })
    )
    
    (ok true)
  )
)

(define-public (pay-maintenance-cost (maintenance-id uint))
  (let
    (
      (request (unwrap! (map-get? maintenance-requests { maintenance-id: maintenance-id }) ERR_MAINTENANCE_NOT_FOUND))
    )
    (asserts! (is-eq (get status request) "completed") ERR_INVALID_STATUS)
    (asserts! (> (get actual-cost request) u0) ERR_INVALID_COST)
    
    (if (is-eq (get responsible-party request) "tenant")
      (begin
        (asserts! (is-eq tx-sender (get tenant request)) ERR_UNAUTHORIZED)
        (try! (stx-transfer? (get actual-cost request) tx-sender (get landlord request)))
      )
      (begin
        (asserts! (is-eq tx-sender (get landlord request)) ERR_UNAUTHORIZED)
        (try! (stx-transfer? (get actual-cost request) tx-sender (get tenant request)))
      )
    )
    
    (map-set maintenance-requests
      { maintenance-id: maintenance-id }
      (merge request { status: "paid" })
    )
    
    (ok true)
  )
)

(define-read-only (get-maintenance-request (maintenance-id uint))
  (map-get? maintenance-requests { maintenance-id: maintenance-id })
)

(define-read-only (get-property-maintenance-history (property-id uint))
  (map-get? property-maintenance-history { property-id: property-id })
)

(define-read-only (get-lease-maintenance-adjustments (lease-id uint))
  (map-get? lease-maintenance-adjustments { lease-id: lease-id })
)

(define-read-only (get-adjusted-lease-terms (lease-id uint))
  (match (map-get? leases { lease-id: lease-id })
    lease 
      (match (map-get? lease-maintenance-adjustments { lease-id: lease-id })
        adjustments (ok {
          original-monthly-rent: (get monthly-rent lease),
          original-total-price: (get total-price lease),
          adjusted-monthly-rent: (+ (get monthly-rent lease) (get rent-adjustments adjustments)),
          adjusted-total-price: (+ (get total-price lease) (get price-adjustments adjustments)),
          total-maintenance-by-tenant: (get total-tenant-costs adjustments),
          total-maintenance-by-landlord: (get total-landlord-costs adjustments)
        })
        (ok {
          original-monthly-rent: (get monthly-rent lease),
          original-total-price: (get total-price lease),
          adjusted-monthly-rent: (get monthly-rent lease),
          adjusted-total-price: (get total-price lease),
          total-maintenance-by-tenant: u0,
          total-maintenance-by-landlord: u0
        })
      )
    ERR_LEASE_NOT_FOUND
  )
)

(define-read-only (get-maintenance-status-summary (property-id uint))
  (match (map-get? property-maintenance-history { property-id: property-id })
    history (ok {
      total-requests: (len (get maintenance-ids history)),
      total-maintenance-cost: (get total-maintenance-cost history),
      request-ids: (get maintenance-ids history)
    })
    (ok {
      total-requests: u0,
      total-maintenance-cost: u0,
      request-ids: (list)
    })
  )
)

(define-read-only (get-next-maintenance-id)
  (var-get next-maintenance-id)
)


