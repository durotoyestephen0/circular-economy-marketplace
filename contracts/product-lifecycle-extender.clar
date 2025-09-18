;; Product Lifecycle Extender Contract
;; Tracks product usage and condition throughout its lifecycle with IoT integration
;; Handles repair service coordination and parts marketplace for extending product life

;; Error constants
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-invalid-condition (err u103))
(define-constant err-insufficient-payment (err u104))
(define-constant err-service-not-available (err u105))
(define-constant err-unauthorized (err u106))
(define-constant err-invalid-score (err u107))

;; Contract owner
(define-constant contract-owner tx-sender)

;; Data structures
(define-map products
    { product-id: uint }
    {
        owner: principal,
        manufacturer: principal,
        product-type: (string-ascii 50),
        manufacturing-date: uint,
        condition-score: uint,
        usage-hours: uint,
        repair-count: uint,
        material-composition: (string-ascii 200),
        sustainability-score: uint,
        is-active: bool
    }
)

(define-map iot-sensors
    { sensor-id: uint }
    {
        product-id: uint,
        sensor-type: (string-ascii 30),
        last-reading: uint,
        status: (string-ascii 20),
        installation-date: uint
    }
)

(define-map repair-services
    { service-id: uint }
    {
        provider: principal,
        service-type: (string-ascii 50),
        price: uint,
        estimated-duration: uint,
        certification-level: uint,
        availability: bool,
        rating: uint
    }
)

(define-map service-requests
    { request-id: uint }
    {
        product-id: uint,
        customer: principal,
        service-id: uint,
        status: (string-ascii 20),
        request-date: uint,
        completion-date: uint,
        payment-amount: uint
    }
)

(define-map parts-marketplace
    { part-id: uint }
    {
        seller: principal,
        part-name: (string-ascii 50),
        compatible-products: (list 10 uint),
        condition: (string-ascii 20),
        price: uint,
        quantity-available: uint,
        certification: (string-ascii 30)
    }
)

(define-map quality-certifications
    { certification-id: uint }
    {
        product-id: uint,
        certifier: principal,
        certification-type: (string-ascii 40),
        issue-date: uint,
        expiry-date: uint,
        grade: (string-ascii 10),
        notes: (string-ascii 100)
    }
)

;; Data counters
(define-data-var next-product-id uint u1)
(define-data-var next-sensor-id uint u1)
(define-data-var next-service-id uint u1)
(define-data-var next-request-id uint u1)
(define-data-var next-part-id uint u1)
(define-data-var next-certification-id uint u1)

;; Product registration functions
(define-public (register-product 
    (manufacturer principal)
    (product-type (string-ascii 50))
    (material-composition (string-ascii 200)))
    (let ((product-id (var-get next-product-id)))
        (map-set products
            { product-id: product-id }
            {
                owner: tx-sender,
                manufacturer: manufacturer,
                product-type: product-type,
                manufacturing-date: block-height,
                condition-score: u100,
                usage-hours: u0,
                repair-count: u0,
                material-composition: material-composition,
                sustainability-score: u100,
                is-active: true
            }
        )
        (var-set next-product-id (+ product-id u1))
        (ok product-id)
    )
)

;; IoT sensor management
(define-public (register-iot-sensor 
    (product-id uint)
    (sensor-type (string-ascii 30)))
    (let ((sensor-id (var-get next-sensor-id)))
        (match (map-get? products { product-id: product-id })
            product (begin
                (map-set iot-sensors
                    { sensor-id: sensor-id }
                    {
                        product-id: product-id,
                        sensor-type: sensor-type,
                        last-reading: u0,
                        status: "active",
                        installation-date: block-height
                    }
                )
                (var-set next-sensor-id (+ sensor-id u1))
                (ok sensor-id)
            )
            err-not-found
        )
    )
)

(define-public (update-sensor-reading 
    (sensor-id uint)
    (reading uint))
    (match (map-get? iot-sensors { sensor-id: sensor-id })
        sensor (begin
            (map-set iot-sensors
                { sensor-id: sensor-id }
                (merge sensor { last-reading: reading })
            )
            (update-product-condition (get product-id sensor) reading)
        )
        err-not-found
    )
)

;; Product condition and usage tracking
(define-private (update-product-condition (product-id uint) (sensor-reading uint))
    (match (map-get? products { product-id: product-id })
        product (let 
            (
                (new-usage-hours (+ (get usage-hours product) u1))
                (condition-adjustment (if (< sensor-reading u50) u5 u1))
                (new-condition (if (> (- (get condition-score product) condition-adjustment) u1) 
                               (- (get condition-score product) condition-adjustment) 
                               u1))
                (new-sustainability (calculate-sustainability-score new-condition new-usage-hours (get repair-count product)))
            )
            (map-set products
                { product-id: product-id }
                (merge product {
                    usage-hours: new-usage-hours,
                    condition-score: new-condition,
                    sustainability-score: new-sustainability
                })
            )
            (ok true)
        )
        err-not-found
    )
)

(define-private (calculate-sustainability-score (condition uint) (usage-hours uint) (repair-count uint))
    (let 
        (
            (condition-factor (/ (* condition u100) u100))
            (usage-factor (if (> usage-hours u1000) u80 u100))
            (repair-bonus (if (< (* repair-count u5) u20) 
                            (* repair-count u5) 
                            u20))
        )
        (if (< (+ condition-factor usage-factor repair-bonus) u100) 
            (+ condition-factor usage-factor repair-bonus) 
            u100)
    )
)

;; Repair service marketplace
(define-public (register-repair-service 
    (service-type (string-ascii 50))
    (price uint)
    (estimated-duration uint)
    (certification-level uint))
    (let ((service-id (var-get next-service-id)))
        (map-set repair-services
            { service-id: service-id }
            {
                provider: tx-sender,
                service-type: service-type,
                price: price,
                estimated-duration: estimated-duration,
                certification-level: certification-level,
                availability: true,
                rating: u50
            }
        )
        (var-set next-service-id (+ service-id u1))
        (ok service-id)
    )
)

(define-public (request-repair-service 
    (product-id uint)
    (service-id uint)
    (payment-amount uint))
    (let ((request-id (var-get next-request-id)))
        (match (map-get? repair-services { service-id: service-id })
            service (if (>= payment-amount (get price service))
                (begin
                    (map-set service-requests
                        { request-id: request-id }
                        {
                            product-id: product-id,
                            customer: tx-sender,
                            service-id: service-id,
                            status: "pending",
                            request-date: block-height,
                            completion-date: u0,
                            payment-amount: payment-amount
                        }
                    )
                    (var-set next-request-id (+ request-id u1))
                    (ok request-id)
                )
                err-insufficient-payment
            )
            err-not-found
        )
    )
)

(define-public (complete-repair-service (request-id uint))
    (match (map-get? service-requests { request-id: request-id })
        request (match (map-get? repair-services { service-id: (get service-id request) })
            service (if (is-eq tx-sender (get provider service))
                (begin
                    (map-set service-requests
                        { request-id: request-id }
                        (merge request {
                            status: "completed",
                            completion-date: block-height
                        })
                    )
                    (increment-repair-count (get product-id request))
                )
                err-unauthorized
            )
            err-not-found
        )
        err-not-found
    )
)

(define-private (increment-repair-count (product-id uint))
    (match (map-get? products { product-id: product-id })
        product (let 
            (
                (new-repair-count (+ (get repair-count product) u1))
                (improved-condition (if (< (+ (get condition-score product) u15) u100) 
                                     (+ (get condition-score product) u15) 
                                     u100))
                (new-sustainability (calculate-sustainability-score improved-condition (get usage-hours product) new-repair-count))
            )
            (map-set products
                { product-id: product-id }
                (merge product {
                    repair-count: new-repair-count,
                    condition-score: improved-condition,
                    sustainability-score: new-sustainability
                })
            )
            (ok true)
        )
        err-not-found
    )
)

;; Parts marketplace functions
(define-public (list-part 
    (part-name (string-ascii 50))
    (compatible-products (list 10 uint))
    (condition (string-ascii 20))
    (price uint)
    (quantity uint)
    (certification (string-ascii 30)))
    (let ((part-id (var-get next-part-id)))
        (map-set parts-marketplace
            { part-id: part-id }
            {
                seller: tx-sender,
                part-name: part-name,
                compatible-products: compatible-products,
                condition: condition,
                price: price,
                quantity-available: quantity,
                certification: certification
            }
        )
        (var-set next-part-id (+ part-id u1))
        (ok part-id)
    )
)

;; Quality certification functions
(define-public (issue-quality-certification 
    (product-id uint)
    (certification-type (string-ascii 40))
    (expiry-date uint)
    (grade (string-ascii 10))
    (notes (string-ascii 100)))
    (let ((cert-id (var-get next-certification-id)))
        (map-set quality-certifications
            { certification-id: cert-id }
            {
                product-id: product-id,
                certifier: tx-sender,
                certification-type: certification-type,
                issue-date: block-height,
                expiry-date: expiry-date,
                grade: grade,
                notes: notes
            }
        )
        (var-set next-certification-id (+ cert-id u1))
        (ok cert-id)
    )
)

;; Read-only functions
(define-read-only (get-product (product-id uint))
    (map-get? products { product-id: product-id })
)

(define-read-only (get-product-sustainability-score (product-id uint))
    (match (map-get? products { product-id: product-id })
        product (ok (get sustainability-score product))
        err-not-found
    )
)

(define-read-only (get-repair-service (service-id uint))
    (map-get? repair-services { service-id: service-id })
)

(define-read-only (get-service-request (request-id uint))
    (map-get? service-requests { request-id: request-id })
)

(define-read-only (get-part-listing (part-id uint))
    (map-get? parts-marketplace { part-id: part-id })
)

(define-read-only (get-quality-certification (certification-id uint))
    (map-get? quality-certifications { certification-id: certification-id })
)

(define-read-only (get-iot-sensor (sensor-id uint))
    (map-get? iot-sensors { sensor-id: sensor-id })
)

;; Admin functions
(define-public (update-service-availability (service-id uint) (available bool))
    (match (map-get? repair-services { service-id: service-id })
        service (if (is-eq tx-sender (get provider service))
            (begin
                (map-set repair-services
                    { service-id: service-id }
                    (merge service { availability: available })
                )
                (ok true)
            )
            err-unauthorized
        )
        err-not-found
    )
)

(define-public (transfer-product-ownership (product-id uint) (new-owner principal))
    (match (map-get? products { product-id: product-id })
        product (if (is-eq tx-sender (get owner product))
            (begin
                (map-set products
                    { product-id: product-id }
                    (merge product { owner: new-owner })
                )
                (ok true)
            )
            err-unauthorized
        )
        err-not-found
    )
)
