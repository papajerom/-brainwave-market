;; Brainwave Market
;; This contract enables peer-to-peer knowledge transfer with reputation tracking and proposal systems


;; =============================================================================
;; REPUTATION SYSTEM
;; =============================================================================

;; Reputation tracking storage
(define-map participant-evaluations {knowledge-provider: principal, evaluator: principal} uint)
(define-map evaluation-frequency principal uint)
(define-map evaluation-aggregate principal uint)

;; =============================================================================
;; EXCHANGE PROPOSAL FRAMEWORK
;; =============================================================================

;; Proposal state tracking
(define-map knowledge-transfer-proposals
  {proposal-identifier: uint}
  {
    initiator: principal,
    provider: principal,
    duration: uint,
    valuation: uint,
    state: uint, ;; 0=awaiting, 1=confirmed, 2=declined, 3=fulfilled
    timestamp: uint
  }
)
(define-data-var proposal-counter uint u1)

;; =============================================================================
;; ADMIN CONFIGURATION 
;; =============================================================================

;; Contract administrator identity
(define-constant admin-identity tx-sender)

;; Error codes for administrative functions
(define-constant error-not-authorized (err u200))
(define-constant error-parameter-invalid (err u212))
(define-constant error-boundary-exceeded (err u213))
(define-constant error-capacity-limit (err u214))

;; Platform configuration variables
(define-data-var hourly-token-value uint u10)
(define-data-var participant-hourly-cap uint u100)
(define-data-var platform-commission-percentage uint u10)
(define-data-var global-capacity-ceiling uint u1000)

;; =============================================================================
;; PARTICIPANT STATE TRACKING
;; =============================================================================

;; Error codes for participant operations
(define-constant error-token-deficiency (err u201))
(define-constant error-duration-invalid (err u202))
(define-constant error-valuation-invalid (err u203))
(define-constant error-capacity-reached (err u204))
(define-constant error-permission-denied (err u205))
(define-constant error-input-range-violation (err u215))
(define-constant error-zero-value-denied (err u210))
(define-constant error-quota-exceeded (err u211))

;; Participant data storage
(define-data-var cumulative-knowledge-capacity uint u0)
(define-map participant-knowledge-inventory principal uint)
(define-map participant-token-inventory principal uint)
(define-map knowledge-marketplace {participant: principal} {duration: uint, valuation: uint})

;; =============================================================================
;; UTILITY FUNCTIONS
;; =============================================================================

;; Calculate platform commission on transactions
(define-private (compute-transaction-commission (value uint))
  (/ (* value (var-get platform-commission-percentage)) u100))

;; Update global knowledge capacity tracking
(define-private (adjust-global-capacity (adjustment int))
  (let (
    (existing-capacity (var-get cumulative-knowledge-capacity))
    (updated-capacity (if (< adjustment 0)
                     (if (>= existing-capacity (to-uint (- 0 adjustment)))
                         (- existing-capacity (to-uint (- 0 adjustment)))
                         u0)
                     (+ existing-capacity (to-uint adjustment))))
  )
    (asserts! (<= updated-capacity (var-get global-capacity-ceiling)) error-capacity-reached)
    (var-set cumulative-knowledge-capacity updated-capacity)
    (ok true)))

;; =============================================================================
;; MARKETPLACE FUNCTIONS
;; =============================================================================

;; Publish knowledge availability to marketplace
(define-public (publish-knowledge-offering (duration uint) (valuation uint))
  (let (
    (available-inventory (default-to u0 (map-get? participant-knowledge-inventory tx-sender)))
    (current-offering (get duration (default-to {duration: u0, valuation: u0} 
                                    (map-get? knowledge-marketplace {participant: tx-sender}))))
    (updated-offering (+ duration current-offering))
  )
    (asserts! (> duration u0) error-duration-invalid)
    (asserts! (> valuation u0) error-valuation-invalid)
    (asserts! (>= available-inventory updated-offering) error-token-deficiency)
    (try! (adjust-global-capacity (to-int duration)))
    (map-set knowledge-marketplace {participant: tx-sender} 
             {duration: updated-offering, valuation: valuation})
    (ok true)))

;; Remove knowledge from marketplace
(define-public (withdraw-knowledge-offering (duration uint))
  (let (
    (current-offering (get duration (default-to {duration: u0, valuation: u0} 
                                    (map-get? knowledge-marketplace {participant: tx-sender}))))
  )
    (asserts! (>= current-offering duration) error-token-deficiency)
    (try! (adjust-global-capacity (to-int (- duration))))
    (map-set knowledge-marketplace {participant: tx-sender} 
             {duration: (- current-offering duration), 
              valuation: (get valuation (default-to {duration: u0, valuation: u0} 
                                       (map-get? knowledge-marketplace {participant: tx-sender})))})
    (ok true)))

;; Direct knowledge exchange execution
(define-public (acquire-knowledge (provider principal) (duration uint))
  (let (
    (offering-data (default-to {duration: u0, valuation: u0} 
                   (map-get? knowledge-marketplace {participant: provider})))
    (transaction-value (* duration (get valuation offering-data)))
    (platform-fee (compute-transaction-commission transaction-value))
    (total-transaction-cost (+ transaction-value platform-fee))
    (provider-inventory (default-to u0 (map-get? participant-knowledge-inventory provider)))
    (acquirer-tokens (default-to u0 (map-get? participant-token-inventory tx-sender)))
    (provider-tokens (default-to u0 (map-get? participant-token-inventory provider)))
  )
    (asserts! (not (is-eq tx-sender provider)) error-permission-denied)
    (asserts! (> duration u0) error-duration-invalid)
    (asserts! (>= (get duration offering-data) duration) error-token-deficiency)
    (asserts! (>= provider-inventory duration) error-token-deficiency)
    (asserts! (>= acquirer-tokens total-transaction-cost) error-token-deficiency)

    ;; Update provider knowledge inventory
    (map-set participant-knowledge-inventory provider (- provider-inventory duration))
    (map-set knowledge-marketplace {participant: provider} 
             {duration: (- (get duration offering-data) duration), 
              valuation: (get valuation offering-data)})

    ;; Update financial transactions
    (map-set participant-token-inventory tx-sender (- acquirer-tokens total-transaction-cost))
    (map-set participant-knowledge-inventory tx-sender 
             (+ (default-to u0 (map-get? participant-knowledge-inventory tx-sender)) duration))

    ;; Credit tokens to provider
    (map-set participant-token-inventory provider (+ provider-tokens transaction-value))

    ;; Record platform fee
    (map-set participant-token-inventory admin-identity 
             (+ (default-to u0 (map-get? participant-token-inventory admin-identity)) platform-fee))

    (ok true)))

;; =============================================================================
;; ADMINISTRATIVE FUNCTIONS
;; =============================================================================

;; Update system parameters
;; Allows administrator to modify platform configuration
;; @param updated-token-value: new base value for knowledge tokens
;; @param updated-cap: new maximum hours per participant
;; @param updated-commission: new platform commission percentage
;; @param updated-ceiling: new global capacity limit
(define-public (configure-platform-parameters 
                (updated-token-value uint) 
                (updated-cap uint) 
                (updated-commission uint) 
                (updated-ceiling uint))
  (begin
    (asserts! (is-eq tx-sender admin-identity) error-not-authorized)
    (asserts! (<= updated-commission u100) error-parameter-invalid)
    (asserts! (> updated-token-value u0) error-valuation-invalid)
    (asserts! (> updated-cap u0) error-boundary-exceeded)
    (asserts! (>= updated-ceiling (var-get cumulative-knowledge-capacity)) error-capacity-limit)

    (var-set hourly-token-value updated-token-value)
    (var-set participant-hourly-cap updated-cap)
    (var-set platform-commission-percentage updated-commission)
    (var-set global-capacity-ceiling updated-ceiling)

    (ok true)))

;; =============================================================================
;; REPUTATION MANAGEMENT
;; =============================================================================

;; Submit evaluation for knowledge provider
;; Allows participants to rate providers after knowledge exchange
;; @param provider: the principal of the provider being evaluated
;; @param score: the evaluation score (1-5) given to the provider
(define-public (evaluate-provider (provider principal) (score uint))
  (let (
    (evaluator tx-sender)
    (existing-evaluation (default-to u0 (map-get? participant-evaluations 
                                        {knowledge-provider: provider, evaluator: evaluator})))
    (current-count (default-to u0 (map-get? evaluation-frequency provider)))
    (current-total (default-to u0 (map-get? evaluation-aggregate provider)))
    (adjusted-count (if (is-eq existing-evaluation u0) (+ current-count u1) current-count))
    (adjusted-total (+ (- current-total existing-evaluation) score))
  )
    (asserts! (not (is-eq evaluator provider)) error-permission-denied)
    (asserts! (and (>= score u1) (<= score u5)) error-input-range-violation)

    ;; Update reputation data
    (map-set participant-evaluations {knowledge-provider: provider, evaluator: evaluator} score)
    (map-set evaluation-frequency provider adjusted-count)
    (map-set evaluation-aggregate provider adjusted-total)

    (ok true)))

;; =============================================================================
;; PROPOSAL SYSTEM
;; =============================================================================

;; Initiate knowledge exchange proposal
;; Creates a formal proposal for knowledge exchange between participants
;; @param provider: principal of the knowledge provider
;; @param duration: requested knowledge exchange duration
;; @param offered-valuation: rate offered for the exchange
(define-public (initiate-knowledge-proposal (provider principal) (duration uint) (offered-valuation uint))
  (let (
    (initiator tx-sender)
    (proposal-id (var-get proposal-counter))
    (offering-data (default-to {duration: u0, valuation: u0} 
                   (map-get? knowledge-marketplace {participant: provider})))
    (transaction-value (* duration offered-valuation))
    (platform-fee (compute-transaction-commission transaction-value))
    (total-cost (+ transaction-value platform-fee))
    (initiator-balance (default-to u0 (map-get? participant-token-inventory initiator)))
  )
    (asserts! (not (is-eq initiator provider)) error-permission-denied)
    (asserts! (> duration u0) error-duration-invalid)
    (asserts! (>= (get duration offering-data) duration) error-token-deficiency)
    (asserts! (> offered-valuation u0) error-valuation-invalid)
    (asserts! (>= initiator-balance total-cost) error-token-deficiency)

    ;; Register the proposal
    (map-set knowledge-transfer-proposals
      {proposal-identifier: proposal-id}
      {
        initiator: initiator,
        provider: provider,
        duration: duration,
        valuation: offered-valuation,
        state: u0, ;; awaiting response
        timestamp: block-height
      }
    )

    ;; Reserve tokens for the proposal
    (map-set participant-token-inventory initiator (- initiator-balance total-cost))

    ;; Increment proposal identifier
    (var-set proposal-counter (+ proposal-id u1))

    (ok proposal-id)))

;; =============================================================================
;; PARTICIPANT ONBOARDING
;; =============================================================================

;; Register knowledge capacity
;; Allows participants to declare knowledge hours available for exchange
;; @param duration: number of knowledge hours to register
(define-public (register-knowledge-capacity (duration uint))
  (let (
    (current-inventory (default-to u0 (map-get? participant-knowledge-inventory tx-sender)))
    (maximum-allowed (var-get participant-hourly-cap))
    (new-inventory (+ current-inventory duration))
  )
    (asserts! (> duration u0) error-duration-invalid)
    (asserts! (<= new-inventory maximum-allowed) error-quota-exceeded)
    (map-set participant-knowledge-inventory tx-sender new-inventory)
    (ok new-inventory)))

;; =============================================================================
;; FINANCIAL OPERATIONS
;; =============================================================================

;; Deposit tokens to participant account
;; Allows participants to add tokens for future knowledge exchanges
;; @param amount: token quantity to deposit
(define-public (deposit-tokens (amount uint))
  (let (
    (participant tx-sender)
    (current-balance (default-to u0 (map-get? participant-token-inventory participant)))
    (updated-balance (+ current-balance amount))
  )
    (asserts! (> amount u0) error-zero-value-denied)
    (try! (stx-transfer? amount participant (as-contract tx-sender)))
    (map-set participant-token-inventory participant updated-balance)
    (ok updated-balance)))

