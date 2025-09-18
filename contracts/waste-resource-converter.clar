;; Waste Resource Converter Contract
;; Manages waste collection and sorting with automated material identification
;; Handles waste-to-resource conversion tracking with value calculation

;; Error constants
(define-constant err-owner-only (err u200))
(define-constant err-not-found (err u201))
(define-constant err-already-exists (err u202))
(define-constant err-invalid-quantity (err u203))
(define-constant err-insufficient-balance (err u204))
(define-constant err-unauthorized (err u205))
(define-constant err-invalid-material (err u206))
(define-constant err-processing-error (err u207))

;; Contract owner
(define-constant contract-owner tx-sender)

;; Data structures
(define-map waste-collections
    { collection-id: uint }
    {
        collector: principal,
        waste-type: (string-ascii 50),
        quantity: uint,
        location: (string-ascii 100),
        collection-date: uint,
        material-composition: (string-ascii 200),
        quality-grade: (string-ascii 10),
        processing-status: (string-ascii 20),
        estimated-value: uint
    }
)

(define-map material-identifiers
    { identifier-id: uint }
    {
        operator: principal,
        technology-type: (string-ascii 40),
        accuracy-rating: uint,
        materials-detected: (list 20 (string-ascii 30)),
        installation-date: uint,
        last-calibration: uint,
        status: (string-ascii 20)
    }
)

(define-map conversion-processes
    { process-id: uint }
    {
        waste-collection-id: uint,
        process-type: (string-ascii 50),
        input-materials: (list 10 (string-ascii 30)),
        output-resources: (list 10 (string-ascii 30)),
        conversion-efficiency: uint,
        start-date: uint,
        completion-date: uint,
        operator: principal,
        value-created: uint
    }
)

(define-map incentive-programs
    { program-id: uint }
    {
        program-name: (string-ascii 60),
        administrator: principal,
        reward-rate: uint,
        eligible-materials: (list 15 (string-ascii 30)),
        minimum-quantity: uint,
        total-budget: uint,
        distributed-rewards: uint,
        active: bool,
        start-date: uint,
        end-date: uint
    }
)

(define-map participant-rewards
    { participant: principal, program-id: uint }
    {
        total-contributions: uint,
        earned-rewards: uint,
        last-contribution-date: uint,
        material-types-contributed: (list 10 (string-ascii 30))
    }
)

(define-map resource-marketplace
    { resource-id: uint }
    {
        seller: principal,
        resource-type: (string-ascii 50),
        quantity-available: uint,
        price-per-unit: uint,
        quality-certification: (string-ascii 30),
        source-process-id: uint,
        listing-date: uint,
        sustainability-score: uint
    }
)

(define-map circular-economy-metrics
    { metric-id: uint }
    {
        entity: principal,
        entity-type: (string-ascii 20),
        waste-diverted: uint,
        resources-recovered: uint,
        carbon-footprint-reduced: uint,
        economic-value-created: uint,
        measurement-period: uint,
        verification-status: (string-ascii 20)
    }
)

;; Data counters
(define-data-var next-collection-id uint u1)
(define-data-var next-identifier-id uint u1)
(define-data-var next-process-id uint u1)
(define-data-var next-program-id uint u1)
(define-data-var next-resource-id uint u1)
(define-data-var next-metric-id uint u1)

;; Waste collection and identification functions
(define-public (register-waste-collection 
    (waste-type (string-ascii 50))
    (quantity uint)
    (location (string-ascii 100))
    (material-composition (string-ascii 200)))
    (let ((collection-id (var-get next-collection-id)))
        (map-set waste-collections
            { collection-id: collection-id }
            {
                collector: tx-sender,
                waste-type: waste-type,
                quantity: quantity,
                location: location,
                collection-date: block-height,
                material-composition: material-composition,
                quality-grade: "pending",
                processing-status: "collected",
                estimated-value: (calculate-waste-value waste-type quantity)
            }
        )
        (var-set next-collection-id (+ collection-id u1))
        (ok collection-id)
    )
)

(define-public (register-material-identifier 
    (technology-type (string-ascii 40))
    (accuracy-rating uint)
    (materials-detected (list 20 (string-ascii 30))))
    (let ((identifier-id (var-get next-identifier-id)))
        (map-set material-identifiers
            { identifier-id: identifier-id }
            {
                operator: tx-sender,
                technology-type: technology-type,
                accuracy-rating: accuracy-rating,
                materials-detected: materials-detected,
                installation-date: block-height,
                last-calibration: block-height,
                status: "active"
            }
        )
        (var-set next-identifier-id (+ identifier-id u1))
        (ok identifier-id)
    )
)

(define-public (process-material-identification 
    (collection-id uint)
    (identifier-id uint)
    (identified-materials (list 10 (string-ascii 30)))
    (quality-grade (string-ascii 10)))
    (match (map-get? waste-collections { collection-id: collection-id })
        collection (match (map-get? material-identifiers { identifier-id: identifier-id })
            identifier (begin
                (map-set waste-collections
                    { collection-id: collection-id }
                    (merge collection {
                        quality-grade: quality-grade,
                        processing-status: "identified"
                    })
                )
                (ok true)
            )
            err-not-found
        )
        err-not-found
    )
)

;; Waste-to-resource conversion functions
(define-public (initiate-conversion-process 
    (waste-collection-id uint)
    (process-type (string-ascii 50))
    (input-materials (list 10 (string-ascii 30)))
    (expected-outputs (list 10 (string-ascii 30))))
    (let ((process-id (var-get next-process-id)))
        (match (map-get? waste-collections { collection-id: waste-collection-id })
            collection (begin
                (map-set conversion-processes
                    { process-id: process-id }
                    {
                        waste-collection-id: waste-collection-id,
                        process-type: process-type,
                        input-materials: input-materials,
                        output-resources: expected-outputs,
                        conversion-efficiency: u0,
                        start-date: block-height,
                        completion-date: u0,
                        operator: tx-sender,
                        value-created: u0
                    }
                )
                (map-set waste-collections
                    { collection-id: waste-collection-id }
                    (merge collection { processing-status: "converting" })
                )
                (var-set next-process-id (+ process-id u1))
                (ok process-id)
            )
            err-not-found
        )
    )
)

(define-public (complete-conversion-process 
    (process-id uint)
    (actual-outputs (list 10 (string-ascii 30)))
    (conversion-efficiency uint)
    (value-created uint))
    (match (map-get? conversion-processes { process-id: process-id })
        process (if (is-eq tx-sender (get operator process))
            (begin
                (map-set conversion-processes
                    { process-id: process-id }
                    (merge process {
                        output-resources: actual-outputs,
                        conversion-efficiency: conversion-efficiency,
                        completion-date: block-height,
                        value-created: value-created
                    })
                )
                (update-collection-status (get waste-collection-id process) "converted")
            )
            err-unauthorized
        )
        err-not-found
    )
)

(define-private (update-collection-status (collection-id uint) (new-status (string-ascii 20)))
    (match (map-get? waste-collections { collection-id: collection-id })
        collection (begin
            (map-set waste-collections
                { collection-id: collection-id }
                (merge collection { processing-status: new-status })
            )
            (ok true)
        )
        err-not-found
    )
)

;; Incentive distribution functions
(define-public (create-incentive-program 
    (program-name (string-ascii 60))
    (reward-rate uint)
    (eligible-materials (list 15 (string-ascii 30)))
    (minimum-quantity uint)
    (total-budget uint)
    (end-date uint))
    (let ((program-id (var-get next-program-id)))
        (map-set incentive-programs
            { program-id: program-id }
            {
                program-name: program-name,
                administrator: tx-sender,
                reward-rate: reward-rate,
                eligible-materials: eligible-materials,
                minimum-quantity: minimum-quantity,
                total-budget: total-budget,
                distributed-rewards: u0,
                active: true,
                start-date: block-height,
                end-date: end-date
            }
        )
        (var-set next-program-id (+ program-id u1))
        (ok program-id)
    )
)

(define-public (distribute-incentive 
    (participant principal)
    (program-id uint)
    (contribution-quantity uint)
    (material-type (string-ascii 30)))
    (match (map-get? incentive-programs { program-id: program-id })
        program (if (and (get active program) (>= contribution-quantity (get minimum-quantity program)))
            (let 
                (
                    (reward-amount (calculate-reward (get reward-rate program) contribution-quantity))
                    (current-rewards (default-to 
                        { total-contributions: u0, earned-rewards: u0, last-contribution-date: u0, material-types-contributed: (list) }
                        (map-get? participant-rewards { participant: participant, program-id: program-id })
                    ))
                )
                (if (<= (+ (get distributed-rewards program) reward-amount) (get total-budget program))
                    (begin
                        (map-set participant-rewards
                            { participant: participant, program-id: program-id }
                            {
                                total-contributions: (+ (get total-contributions current-rewards) contribution-quantity),
                                earned-rewards: (+ (get earned-rewards current-rewards) reward-amount),
                                last-contribution-date: block-height,
                                material-types-contributed: (unwrap-panic (as-max-len? (append (get material-types-contributed current-rewards) material-type) u10))
                            }
                        )
                        (map-set incentive-programs
                            { program-id: program-id }
                            (merge program { distributed-rewards: (+ (get distributed-rewards program) reward-amount) })
                        )
                        (ok reward-amount)
                    )
                    err-insufficient-balance
                )
            )
            err-invalid-quantity
        )
        err-not-found
    )
)

;; Resource marketplace functions
(define-public (list-recovered-resource 
    (resource-type (string-ascii 50))
    (quantity uint)
    (price-per-unit uint)
    (quality-certification (string-ascii 30))
    (source-process-id uint))
    (let ((resource-id (var-get next-resource-id)))
        (match (map-get? conversion-processes { process-id: source-process-id })
            process (begin
                (map-set resource-marketplace
                    { resource-id: resource-id }
                    {
                        seller: tx-sender,
                        resource-type: resource-type,
                        quantity-available: quantity,
                        price-per-unit: price-per-unit,
                        quality-certification: quality-certification,
                        source-process-id: source-process-id,
                        listing-date: block-height,
                        sustainability-score: (calculate-sustainability-score (get conversion-efficiency process))
                    }
                )
                (var-set next-resource-id (+ resource-id u1))
                (ok resource-id)
            )
            err-not-found
        )
    )
)

;; Circular economy impact metrics
(define-public (record-impact-metrics 
    (entity-type (string-ascii 20))
    (waste-diverted uint)
    (resources-recovered uint)
    (carbon-footprint-reduced uint)
    (economic-value-created uint))
    (let ((metric-id (var-get next-metric-id)))
        (map-set circular-economy-metrics
            { metric-id: metric-id }
            {
                entity: tx-sender,
                entity-type: entity-type,
                waste-diverted: waste-diverted,
                resources-recovered: resources-recovered,
                carbon-footprint-reduced: carbon-footprint-reduced,
                economic-value-created: economic-value-created,
                measurement-period: block-height,
                verification-status: "pending"
            }
        )
        (var-set next-metric-id (+ metric-id u1))
        (ok metric-id)
    )
)

(define-public (verify-impact-metrics (metric-id uint))
    (match (map-get? circular-economy-metrics { metric-id: metric-id })
        metrics (if (is-eq tx-sender contract-owner)
            (begin
                (map-set circular-economy-metrics
                    { metric-id: metric-id }
                    (merge metrics { verification-status: "verified" })
                )
                (ok true)
            )
            err-unauthorized
        )
        err-not-found
    )
)

;; Helper functions
(define-private (calculate-waste-value (waste-type (string-ascii 50)) (quantity uint))
    (if (is-eq waste-type "plastic")
        (* quantity u10)
        (if (is-eq waste-type "metal")
            (* quantity u20)
            (if (is-eq waste-type "electronic")
                (* quantity u50)
                (* quantity u5)
            )
        )
    )
)

(define-private (calculate-reward (reward-rate uint) (quantity uint))
    (/ (* reward-rate quantity) u100)
)

(define-private (calculate-sustainability-score (efficiency uint))
    (if (>= efficiency u90)
        u100
        (if (>= efficiency u70)
            u80
            (if (>= efficiency u50)
                u60
                u40
            )
        )
    )
)

;; Read-only functions
(define-read-only (get-waste-collection (collection-id uint))
    (map-get? waste-collections { collection-id: collection-id })
)

(define-read-only (get-conversion-process (process-id uint))
    (map-get? conversion-processes { process-id: process-id })
)

(define-read-only (get-incentive-program (program-id uint))
    (map-get? incentive-programs { program-id: program-id })
)

(define-read-only (get-participant-rewards (participant principal) (program-id uint))
    (map-get? participant-rewards { participant: participant, program-id: program-id })
)

(define-read-only (get-resource-listing (resource-id uint))
    (map-get? resource-marketplace { resource-id: resource-id })
)

(define-read-only (get-impact-metrics (metric-id uint))
    (map-get? circular-economy-metrics { metric-id: metric-id })
)

(define-read-only (get-material-identifier (identifier-id uint))
    (map-get? material-identifiers { identifier-id: identifier-id })
)

;; Administrative functions
(define-public (update-program-status (program-id uint) (active bool))
    (match (map-get? incentive-programs { program-id: program-id })
        program (if (is-eq tx-sender (get administrator program))
            (begin
                (map-set incentive-programs
                    { program-id: program-id }
                    (merge program { active: active })
                )
                (ok true)
            )
            err-unauthorized
        )
        err-not-found
    )
)

(define-public (calibrate-material-identifier (identifier-id uint))
    (match (map-get? material-identifiers { identifier-id: identifier-id })
        identifier (if (is-eq tx-sender (get operator identifier))
            (begin
                (map-set material-identifiers
                    { identifier-id: identifier-id }
                    (merge identifier { last-calibration: block-height })
                )
                (ok true)
            )
            err-unauthorized
        )
        err-not-found
    )
)
