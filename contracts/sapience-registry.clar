;; Sapience Contribution Registry
;; A decentralized system for scholarly content preservation, attribution and authorized access management
;; Enables secure cataloging and controlled distribution of academic and research materials

;; System-wide Configuration Constants
(define-constant CONTRACT_OPERATOR tx-sender)
(define-constant ERROR_NOT_AUTHORIZED (err u300))
(define-constant ERROR_ENTRY_NONEXISTENT (err u301))
(define-constant ERROR_DUPLICATE_ENTRY (err u302))
(define-constant ERROR_INVALID_METADATA (err u303))
(define-constant ERROR_INVALID_DOCUMENT_SIZE (err u304))
(define-constant ERROR_PERMISSION_DENIED (err u305))

;; Repository Entry Tracking
(define-data-var entry-counter uint u0)

;; Core Data Structures
(define-map knowledge-repository
  { entry-number: uint }
  {
    document-name: (string-ascii 80),
    document-creator: principal,
    document-byte-count: uint,
    submission-block: uint,
    document-summary: (string-ascii 256),
    document-tags: (list 8 (string-ascii 40))
  }
)

(define-map permission-ledger
  { entry-number: uint, user-address: principal }
  { permission-granted: bool }
)

;; Validation Helper Functions
(define-private (entry-exists (entry-number uint))
  (is-some (map-get? knowledge-repository { entry-number: entry-number }))
)

(define-private (validate-tag-collection (tag-collection (list 8 (string-ascii 40))))
  (and
    (> (len tag-collection) u0)
    (<= (len tag-collection) u8)
    (is-eq (len (filter is-valid-tag tag-collection)) (len tag-collection))
  )
)

(define-private (is-valid-tag (tag (string-ascii 40)))
  (and 
    (> (len tag) u0)
    (< (len tag) u41)
  )
)

(define-private (is-entry-creator (entry-number uint) (creator principal))
  (match (map-get? knowledge-repository { entry-number: entry-number })
    repository-entry (is-eq (get document-creator repository-entry) creator)
    false
  )
)

(define-private (get-document-byte-count (entry-number uint))
  (default-to u0 
    (get document-byte-count 
      (map-get? knowledge-repository { entry-number: entry-number })
    )
  )
)

;; Core Repository Management Functions

;; Creates a new scholarly entry in the vault
(define-public (submit-scholarly-document (document-name (string-ascii 80)) (document-byte-count uint) (document-summary (string-ascii 256)) (document-tags (list 8 (string-ascii 40))))
  (let
    (
      (entry-number (+ (var-get entry-counter) u1))
    )
    ;; Validate document name requirements
    (asserts! (> (len document-name) u0) ERROR_INVALID_METADATA)
    (asserts! (< (len document-name) u81) ERROR_INVALID_METADATA)

    ;; Validate document size constraints
    (asserts! (> document-byte-count u0) ERROR_INVALID_DOCUMENT_SIZE)
    (asserts! (< document-byte-count u2000000000) ERROR_INVALID_DOCUMENT_SIZE)

    ;; Validate summary requirements
    (asserts! (> (len document-summary) u0) ERROR_INVALID_METADATA)
    (asserts! (< (len document-summary) u257) ERROR_INVALID_METADATA)

    ;; Validate tag collection
    (asserts! (validate-tag-collection document-tags) ERROR_INVALID_METADATA)

    ;; Record document in repository
    (map-insert knowledge-repository
      { entry-number: entry-number }
      {
        document-name: document-name,
        document-creator: tx-sender,
        document-byte-count: document-byte-count,
        submission-block: block-height,
        document-summary: document-summary,
        document-tags: document-tags
      }
    )

    ;; Grant access to document creator
    (map-insert permission-ledger
      { entry-number: entry-number, user-address: tx-sender }
      { permission-granted: true }
    )

    ;; Update counter for next entry
    (var-set entry-counter entry-number)
    (ok entry-number)
  )
)

;; An alternative entry submission function with identical behavior but different implementation structure
(define-public (catalog-knowledge-asset (document-name (string-ascii 80)) (document-byte-count uint) (document-summary (string-ascii 256)) (document-tags (list 8 (string-ascii 40))))
  (let
    (
      (entry-number (+ (var-get entry-counter) u1))
    )
    ;; Input validation checks
    (asserts! (> (len document-name) u0) ERROR_INVALID_METADATA)
    (asserts! (< (len document-name) u81) ERROR_INVALID_METADATA)
    (asserts! (> document-byte-count u0) ERROR_INVALID_DOCUMENT_SIZE)
    (asserts! (< document-byte-count u2000000000) ERROR_INVALID_DOCUMENT_SIZE)
    (asserts! (> (len document-summary) u0) ERROR_INVALID_METADATA)
    (asserts! (< (len document-summary) u257) ERROR_INVALID_METADATA)
    (asserts! (validate-tag-collection document-tags) ERROR_INVALID_METADATA)

    ;; Store entry metadata
    (map-insert knowledge-repository
      { entry-number: entry-number }
      {
        document-name: document-name,
        document-creator: tx-sender,
        document-byte-count: document-byte-count,
        submission-block: block-height,
        document-summary: document-summary,
        document-tags: document-tags
      }
    )

    ;; Initialize creator access privileges
    (map-insert permission-ledger
      { entry-number: entry-number, user-address: tx-sender }
      { permission-granted: true }
    )

    ;; Update global sequence tracker
    (var-set entry-counter entry-number)
    (ok entry-number)
  )
)

;; Updates an existing document's metadata
(define-public (revise-document-metadata (entry-number uint) (revised-name (string-ascii 80)) (revised-size uint) (revised-summary (string-ascii 256)) (revised-tags (list 8 (string-ascii 40))))
  (let
    (
      (repository-entry (unwrap! (map-get? knowledge-repository { entry-number: entry-number }) ERROR_ENTRY_NONEXISTENT))
    )
    ;; Verify document existence and ownership
    (asserts! (entry-exists entry-number) ERROR_ENTRY_NONEXISTENT)
    (asserts! (is-eq (get document-creator repository-entry) tx-sender) ERROR_PERMISSION_DENIED)

    ;; Validate revised document name
    (asserts! (> (len revised-name) u0) ERROR_INVALID_METADATA)
    (asserts! (< (len revised-name) u81) ERROR_INVALID_METADATA)

    ;; Validate revised document size
    (asserts! (> revised-size u0) ERROR_INVALID_DOCUMENT_SIZE)
    (asserts! (< revised-size u2000000000) ERROR_INVALID_DOCUMENT_SIZE)

    ;; Validate revised summary
    (asserts! (> (len revised-summary) u0) ERROR_INVALID_METADATA)
    (asserts! (< (len revised-summary) u257) ERROR_INVALID_METADATA)

    ;; Validate revised tags
    (asserts! (validate-tag-collection revised-tags) ERROR_INVALID_METADATA)

    ;; Apply metadata updates
    (map-set knowledge-repository
      { entry-number: entry-number }
      (merge repository-entry { 
        document-name: revised-name, 
        document-byte-count: revised-size, 
        document-summary: revised-summary, 
        document-tags: revised-tags 
      })
    )
    (ok true)
  )
)

;; Removes a document from the knowledge repository
(define-public (withdraw-scholarly-document (entry-number uint))
  (let
    (
      (repository-entry (unwrap! (map-get? knowledge-repository { entry-number: entry-number }) ERROR_ENTRY_NONEXISTENT))
    )
    ;; Verify document exists
    (asserts! (entry-exists entry-number) ERROR_ENTRY_NONEXISTENT)
    ;; Ensure only document creator can remove
    (asserts! (is-eq (get document-creator repository-entry) tx-sender) ERROR_PERMISSION_DENIED)

    ;; Permanently remove document from repository
    (map-delete knowledge-repository { entry-number: entry-number })
    (ok true)
  )
)

;; Query and Display Functions

;; Generates user interface visualization for document details
(define-public (view-document-details (entry-number uint))
  (let
    (
      (repository-entry (unwrap! (map-get? knowledge-repository { entry-number: entry-number }) ERROR_ENTRY_NONEXISTENT))
    )
    ;; Format visualization-friendly structure
    (ok {
      view-title: "Document Portal",
      document-name: (get document-name repository-entry),
      document-creator: (get document-creator repository-entry),
      document-summary: (get document-summary repository-entry),
      document-tags: (get document-tags repository-entry)
    })
  )
)

;; Provides basic document identification data optimized for quick retrieval
(define-public (fetch-essential-metadata (entry-number uint))
  (let
    (
      (repository-entry (unwrap! (map-get? knowledge-repository { entry-number: entry-number }) ERROR_ENTRY_NONEXISTENT))
    )
    ;; Return streamlined identification data
    (ok {
      document-name: (get document-name repository-entry),
      document-creator: (get document-creator repository-entry),
      document-byte-count: (get document-byte-count repository-entry)
    })
  )
)
;; This function is optimized for minimal computational overhead

;; Creates comprehensive document profile for complete information display
(define-public (generate-complete-document-profile (entry-number uint))
  (let
    (
      (repository-entry (unwrap! (map-get? knowledge-repository { entry-number: entry-number }) ERROR_ENTRY_NONEXISTENT))
    )
    ;; Format complete document profile
    (ok {
      name: (get document-name repository-entry),
      creator: (get document-creator repository-entry),
      size: (get document-byte-count repository-entry),
      summary: (get document-summary repository-entry),
      tags: (get document-tags repository-entry)
    })
  )
)

;; Ultra-efficient minimal document identification
(define-public (fetch-document-identity (entry-number uint))
  (let
    (
      (repository-entry (unwrap! (map-get? knowledge-repository { entry-number: entry-number }) ERROR_ENTRY_NONEXISTENT))
    )
    ;; Return absolute minimum document identification
    (ok {
      document-name: (get document-name repository-entry),
      document-creator: (get document-creator repository-entry)
    })
  )
)
;; This performance-optimized function returns only critical identification information

;; Retrieves only the document summary text
(define-public (extract-document-summary (entry-number uint))
  (let
    (
      (repository-entry (unwrap! (map-get? knowledge-repository { entry-number: entry-number }) ERROR_ENTRY_NONEXISTENT))
    )
    (ok (get document-summary repository-entry))
  )
)

;; Validation framework for testing document metadata compliance
(define-public (validate-document-submission-parameters (document-name (string-ascii 80)) (document-byte-count uint) (document-summary (string-ascii 256)) (document-tags (list 8 (string-ascii 40))))
  (begin
    ;; Validate document name format
    (asserts! (> (len document-name) u0) ERROR_INVALID_METADATA)
    (asserts! (< (len document-name) u81) ERROR_INVALID_METADATA)

    ;; Validate document size constraints
    (asserts! (> document-byte-count u0) ERROR_INVALID_DOCUMENT_SIZE)
    (asserts! (< document-byte-count u2000000000) ERROR_INVALID_DOCUMENT_SIZE)

    ;; Validate summary format
    (asserts! (> (len document-summary) u0) ERROR_INVALID_METADATA)
    (asserts! (< (len document-summary) u257) ERROR_INVALID_METADATA)

    ;; Validate tag collection meets repository standards
    (asserts! (validate-tag-collection document-tags) ERROR_INVALID_METADATA)

    (ok true)
  )
)

