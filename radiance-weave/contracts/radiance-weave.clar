;; RadianceWeave - Decentralized Social Impact Platform
;; Simplified Stacks Clarity Implementation

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-insufficient-funds (err u103))
(define-constant err-invalid-milestone (err u104))
(define-constant err-already-exists (err u105))

;; Data Variables
(define-data-var project-nonce uint u0)
(define-data-var total-impact-score uint u0)

;; Data Maps
(define-map projects
    { project-id: uint }
    {
        creator: principal,
        title: (string-ascii 100),
        funding-goal: uint,
        current-funding: uint,
        impact-category: (string-ascii 50),
        milestones-completed: uint,
        total-milestones: uint,
        is-active: bool,
        linked-projects: (list 5 uint),
        impact-score: uint
    }
)

(define-map project-milestones
    { project-id: uint, milestone-id: uint }
    {
        description: (string-ascii 200),
        funding-amount: uint,
        is-completed: bool,
        verification-count: uint,
        required-verifications: uint
    }
)

(define-map project-contributions
    { project-id: uint, contributor: principal }
    { amount: uint, contribution-date: uint }
)

(define-map community-reputation
    { community: principal }
    { 
        reputation-score: uint,
        projects-created: uint,
        projects-completed: uint,
        total-impact-generated: uint
    }
)

(define-map impact-verifications
    { project-id: uint, milestone-id: uint, verifier: principal }
    { verified: bool, verification-date: uint }
)

;; Read-only functions
(define-read-only (get-project (project-id uint))
    (map-get? projects { project-id: project-id })
)

(define-read-only (get-milestone (project-id uint) (milestone-id uint))
    (map-get? project-milestones { project-id: project-id, milestone-id: milestone-id })
)

(define-read-only (get-community-reputation (community principal))
    (default-to 
        { reputation-score: u0, projects-created: u0, projects-completed: u0, total-impact-generated: u0 }
        (map-get? community-reputation { community: community })
    )
)

(define-read-only (get-contribution (project-id uint) (contributor principal))
    (map-get? project-contributions { project-id: project-id, contributor: contributor })
)

(define-read-only (get-total-impact-score)
    (var-get total-impact-score)
)

;; Private functions

;; Update community reputation
(define-private (update-community-reputation 
    (community principal)
    (projects-delta uint)
    (completed-delta uint)
    (impact-delta uint)
)
    (let
        (
            (current-rep (get-community-reputation community))
        )
        (map-set community-reputation
            { community: community }
            {
                reputation-score: (+ (get reputation-score current-rep) impact-delta),
                projects-created: (+ (get projects-created current-rep) projects-delta),
                projects-completed: (+ (get projects-completed current-rep) completed-delta),
                total-impact-generated: (+ (get total-impact-generated current-rep) impact-delta)
            }
        )
        true
    )
)

;; Distribute percentage to linked projects
(define-private (distribute-to-linked-project (linked-project-id uint) (amount uint))
    (let
        (
            (distribution-amount (/ (* amount u5) u100)) ;; 5% distribution
            (linked-project (unwrap! (get-project linked-project-id) err-not-found))
        )
        (if (get is-active linked-project)
            (begin
                (map-set projects
                    { project-id: linked-project-id }
                    (merge linked-project { 
                        current-funding: (+ (get current-funding linked-project) distribution-amount)
                    })
                )
                (ok true)
            )
            (ok false)
        )
    )
)

;; Complete milestone and release funds
(define-private (complete-milestone (project-id uint) (milestone-id uint))
    (let
        (
            (project (unwrap! (get-project project-id) err-not-found))
            (milestone (unwrap! (get-milestone project-id milestone-id) err-not-found))
            (funding-amount (get funding-amount milestone))
        )
        ;; Mark milestone as completed
        (map-set project-milestones
            { project-id: project-id, milestone-id: milestone-id }
            (merge milestone { is-completed: true })
        )
        
        ;; Update project milestones completed
        (map-set projects
            { project-id: project-id }
            (merge project { 
                milestones-completed: (+ (get milestones-completed project) u1),
                impact-score: (+ (get impact-score project) u10)
            })
        )
        
        ;; Release funds to project creator
        (try! (as-contract (stx-transfer? funding-amount tx-sender (get creator project))))
        
        ;; Distribute to linked projects (simplified - 5% to first linked project)
        (let
            (
                (linked-projects (get linked-projects project))
            )
            (if (> (len linked-projects) u0)
                (distribute-to-linked-project (unwrap-panic (element-at linked-projects u0)) funding-amount)
                (ok true)
            )
        )
    )
)

;; Public functions

;; Create a new social impact project
(define-public (create-project 
    (title (string-ascii 100))
    (funding-goal uint)
    (impact-category (string-ascii 50))
    (total-milestones uint)
    (linked-projects (list 5 uint))
)
    (let
        (
            (project-id (+ (var-get project-nonce) u1))
            (creator tx-sender)
        )
        (asserts! (> funding-goal u0) err-insufficient-funds)
        (asserts! (> total-milestones u0) err-invalid-milestone)
        
        ;; Create project
        (map-set projects
            { project-id: project-id }
            {
                creator: creator,
                title: title,
                funding-goal: funding-goal,
                current-funding: u0,
                impact-category: impact-category,
                milestones-completed: u0,
                total-milestones: total-milestones,
                is-active: true,
                linked-projects: linked-projects,
                impact-score: u0
            }
        )
        
        ;; Update creator reputation
        (update-community-reputation creator u1 u0 u0)
        
        ;; Increment nonce
        (var-set project-nonce project-id)
        
        (ok project-id)
    )
)

;; Contribute to a project
(define-public (contribute-to-project (project-id uint) (amount uint))
    (let
        (
            (project (unwrap! (get-project project-id) err-not-found))
            (contributor tx-sender)
            (current-contribution (default-to 
                { amount: u0, contribution-date: u0 }
                (get-contribution project-id contributor)
            ))
        )
        (asserts! (get is-active project) err-unauthorized)
        (asserts! (> amount u0) err-insufficient-funds)
        
        ;; Transfer STX to contract
        (try! (stx-transfer? amount contributor (as-contract tx-sender)))
        
        ;; Update project funding
        (map-set projects
            { project-id: project-id }
            (merge project { current-funding: (+ (get current-funding project) amount) })
        )
        
        ;; Record contribution
        (map-set project-contributions
            { project-id: project-id, contributor: contributor }
            { 
                amount: (+ (get amount current-contribution) amount),
                contribution-date: block-height
            }
        )
        
        (ok true)
    )
)

;; Add milestone to project
(define-public (add-milestone 
    (project-id uint)
    (milestone-id uint)
    (description (string-ascii 200))
    (funding-amount uint)
    (required-verifications uint)
)
    (let
        (
            (project (unwrap! (get-project project-id) err-not-found))
        )
        (asserts! (is-eq tx-sender (get creator project)) err-unauthorized)
        (asserts! (get is-active project) err-unauthorized)
        
        (map-set project-milestones
            { project-id: project-id, milestone-id: milestone-id }
            {
                description: description,
                funding-amount: funding-amount,
                is-completed: false,
                verification-count: u0,
                required-verifications: required-verifications
            }
        )
        
        (ok true)
    )
)

;; Verify milestone completion
(define-public (verify-milestone (project-id uint) (milestone-id uint))
    (let
        (
            (milestone (unwrap! (get-milestone project-id milestone-id) err-not-found))
            (verifier tx-sender)
            (existing-verification (map-get? impact-verifications 
                { project-id: project-id, milestone-id: milestone-id, verifier: verifier }))
        )
        (asserts! (is-none existing-verification) err-already-exists)
        (asserts! (not (get is-completed milestone)) err-invalid-milestone)
        
        ;; Record verification
        (map-set impact-verifications
            { project-id: project-id, milestone-id: milestone-id, verifier: verifier }
            { verified: true, verification-date: block-height }
        )
        
        ;; Update verification count
        (let
            (
                (new-verification-count (+ (get verification-count milestone) u1))
            )
            (map-set project-milestones
                { project-id: project-id, milestone-id: milestone-id }
                (merge milestone { verification-count: new-verification-count })
            )
            
            ;; Check if milestone is complete
            (if (>= new-verification-count (get required-verifications milestone))
                (complete-milestone project-id milestone-id)
                (ok false)
            )
        )
    )
)

;; Complete project (when all milestones done)
(define-public (finalize-project (project-id uint))
    (let
        (
            (project (unwrap! (get-project project-id) err-not-found))
        )
        (asserts! (is-eq tx-sender (get creator project)) err-unauthorized)
        (asserts! (is-eq (get milestones-completed project) (get total-milestones project)) err-invalid-milestone)
        
        ;; Mark project as inactive
        (map-set projects
            { project-id: project-id }
            (merge project { is-active: false })
        )
        
        ;; Update creator reputation
        (update-community-reputation (get creator project) u0 u1 (get impact-score project))
        
        ;; Update global impact score
        (var-set total-impact-score (+ (var-get total-impact-score) (get impact-score project)))
        
        (ok true)
    )
)