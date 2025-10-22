;; CloudMesa Monitoring Smart Contract
;; Decentralized dApp Health Monitoring and Validator Network

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-insufficient-stake (err u101))
(define-constant err-validator-not-found (err u102))
(define-constant err-invalid-health-score (err u103))
(define-constant err-already-validator (err u104))
(define-constant err-no-stake (err u105))

;; Minimum stake required to become a validator (in microSTX)
(define-constant min-validator-stake u1000000)

;; Data Variables
(define-data-var total-validators uint u0)
(define-data-var total-health-reports uint u0)

;; Data Maps

;; Validator registry with staking information
(define-map validators
    principal
    {
        stake-amount: uint,
        active: bool,
        reports-submitted: uint,
        accuracy-score: uint,
        join-height: uint
    }
)

;; Health reports for monitored dApps
(define-map health-reports
    { dapp-id: (string-ascii 64), report-id: uint }
    {
        validator: principal,
        health-score: uint,
        timestamp: uint,
        gas-anomaly: bool,
        congestion-level: uint,
        risk-level: uint
    }
)

;; dApp monitoring configuration
(define-map monitored-dapps
    (string-ascii 64)
    {
        owner: principal,
        active: bool,
        total-reports: uint,
        avg-health-score: uint,
        alert-threshold: uint
    }
)

;; Incident records
(define-map incidents
    uint
    {
        dapp-id: (string-ascii 64),
        severity: uint,
        detected-at: uint,
        resolved: bool,
        auto-response-triggered: bool
    }
)

(define-data-var incident-counter uint u0)

;; Read-only functions

(define-read-only (get-validator (validator principal))
    (map-get? validators validator)
)

(define-read-only (get-health-report (dapp-id (string-ascii 64)) (report-id uint))
    (map-get? health-reports { dapp-id: dapp-id, report-id: report-id })
)

(define-read-only (get-monitored-dapp (dapp-id (string-ascii 64)))
    (map-get? monitored-dapps dapp-id)
)

(define-read-only (get-incident (incident-id uint))
    (map-get? incidents incident-id)
)

(define-read-only (get-total-validators)
    (ok (var-get total-validators))
)

(define-read-only (get-total-reports)
    (ok (var-get total-health-reports))
)

(define-read-only (is-validator (address principal))
    (match (map-get? validators address)
        validator (ok (get active validator))
        (ok false)
    )
)

;; Public functions

;; Validator registration with staking
(define-public (register-validator (stake-amount uint))
    (let
        (
            (validator tx-sender)
        )
        ;; Check minimum stake requirement
        (asserts! (>= stake-amount min-validator-stake) err-insufficient-stake)
        
        ;; Check if already a validator
        (asserts! (is-none (map-get? validators validator)) err-already-validator)
        
        ;; Transfer stake (in production, would use STX transfer)
        ;; For simplicity, we're recording the stake amount
        
        ;; Register validator
        (map-set validators validator {
            stake-amount: stake-amount,
            active: true,
            reports-submitted: u0,
            accuracy-score: u100,
            join-height: block-height
        })
        
        ;; Increment total validators
        (var-set total-validators (+ (var-get total-validators) u1))
        
        (ok true)
    )
)

;; Submit health report
(define-public (submit-health-report 
    (dapp-id (string-ascii 64))
    (health-score uint)
    (gas-anomaly bool)
    (congestion-level uint)
    (risk-level uint))
    (let
        (
            (validator tx-sender)
            (report-id (var-get total-health-reports))
            (dapp-info (unwrap! (map-get? monitored-dapps dapp-id) err-validator-not-found))
        )
        ;; Verify validator is registered and active
        (match (map-get? validators validator)
            validator-info
                (begin
                    (asserts! (get active validator-info) err-validator-not-found)
                    
                    ;; Validate health score (0-100)
                    (asserts! (<= health-score u100) err-invalid-health-score)
                    
                    ;; Store health report
                    (map-set health-reports 
                        { dapp-id: dapp-id, report-id: report-id }
                        {
                            validator: validator,
                            health-score: health-score,
                            timestamp: block-height,
                            gas-anomaly: gas-anomaly,
                            congestion-level: congestion-level,
                            risk-level: risk-level
                        }
                    )
                    
                    ;; Update validator stats
                    (map-set validators validator
                        (merge validator-info { reports-submitted: (+ (get reports-submitted validator-info) u1) })
                    )
                    
                    ;; Update dApp stats
                    (map-set monitored-dapps dapp-id
                        (merge dapp-info { 
                            total-reports: (+ (get total-reports dapp-info) u1),
                            avg-health-score: (/ (+ (* (get avg-health-score dapp-info) (get total-reports dapp-info)) health-score) 
                                                  (+ (get total-reports dapp-info) u1))
                        })
                    )
                    
                    ;; Increment report counter
                    (var-set total-health-reports (+ report-id u1))
                    
                    ;; Check if incident should be created
                    (if (< health-score (get alert-threshold dapp-info))
                        (begin
                            (unwrap-panic (create-incident dapp-id risk-level))
                            (ok report-id)
                        )
                        (ok report-id)
                    )
                )
            err-validator-not-found
        )
    )
)

;; Register a dApp for monitoring
(define-public (register-dapp (dapp-id (string-ascii 64)) (alert-threshold uint))
    (begin
        ;; Store dApp configuration
        (map-set monitored-dapps dapp-id {
            owner: tx-sender,
            active: true,
            total-reports: u0,
            avg-health-score: u100,
            alert-threshold: alert-threshold
        })
        (ok true)
    )
)

;; Create incident record
(define-private (create-incident (dapp-id (string-ascii 64)) (severity uint))
    (let
        (
            (incident-id (var-get incident-counter))
        )
        (map-set incidents incident-id {
            dapp-id: dapp-id,
            severity: severity,
            detected-at: block-height,
            resolved: false,
            auto-response-triggered: (>= severity u7)
        })
        
        (var-set incident-counter (+ incident-id u1))
        (ok incident-id)
    )
)

;; Resolve incident
(define-public (resolve-incident (incident-id uint))
    (let
        (
            (incident-info (unwrap! (map-get? incidents incident-id) err-validator-not-found))
        )
        ;; Update incident status
        (map-set incidents incident-id
            (merge incident-info { resolved: true })
        )
        (ok true)
    )
)

;; Unstake and deregister validator
(define-public (unregister-validator)
    (let
        (
            (validator tx-sender)
            (validator-info (unwrap! (map-get? validators validator) err-validator-not-found))
        )
        (asserts! (> (get stake-amount validator-info) u0) err-no-stake)
        
        ;; Mark validator as inactive
        (map-set validators validator
            (merge validator-info { active: false })
        )
        
        ;; In production, would return staked tokens here
        
        (var-set total-validators (- (var-get total-validators) u1))
        (ok (get stake-amount validator-info))
    )
)

;; Update dApp monitoring status
(define-public (toggle-dapp-monitoring (dapp-id (string-ascii 64)))
    (let
        (
            (dapp-info (unwrap! (map-get? monitored-dapps dapp-id) err-validator-not-found))
        )
        ;; Only dApp owner can toggle
        (asserts! (is-eq tx-sender (get owner dapp-info)) err-owner-only)
        
        (map-set monitored-dapps dapp-id
            (merge dapp-info { active: (not (get active dapp-info)) })
        )
        (ok true)
    )
)

;; Initialize contract
(begin
    (var-set total-validators u0)
    (var-set total-health-reports u0)
    (var-set incident-counter u0)
)