;; Streak Tracker NFT Contract
;; A contract that tracks daily streaks and mints NFTs as rewards

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-OWNER-ONLY (err u100))
(define-constant ERR-NOT-TOKEN-OWNER (err u101))
(define-constant ERR-ALREADY-CHECKED-IN-TODAY (err u102))
(define-constant ERR-STREAK-BROKEN (err u103))
(define-constant ERR-INVALID-TOKEN-ID (err u104))
(define-constant ERR-NOT-FOUND (err u105))
(define-constant ERR-UNAUTHORIZED-OPERATION (err u106))
(define-constant ERR-INVALID-INPUT (err u107))
(define-constant ERR-INVALID-URI (err u108))
(define-constant ERR-INVALID-STREAK-DAYS (err u109))
(define-constant ERR-INVALID-BONUS-POINTS (err u110))
(define-constant BLOCKS-PER-DAY u144) ;; Approximate blocks per day on Stacks
(define-constant MAX-STREAK-DAYS u36500) ;; 100 years max
(define-constant MAX-BONUS-POINTS u100000) ;; Reasonable max bonus points
(define-constant MIN-URI-LENGTH u1)
(define-constant MAX-URI-LENGTH u256)

;; NFT definition
(define-non-fungible-token streak-tracker-nft uint)

;; Data variables
(define-data-var last-token-id uint u0)
(define-data-var contract-uri (optional (string-ascii 256)) none)
(define-data-var contract-is-active bool true)

;; Data maps
(define-map token-metadata uint (string-ascii 256))
(define-map user-habit-statistics 
  principal 
  {
    active-streak-count: uint,
    best-streak-record: uint,
    last-activity-block: uint,
    lifetime-check-ins: uint,
    account-creation-block: uint
  })
(define-map streak-achievement-rewards 
  uint 
  {
    metadata-uri: (string-ascii 256),
    bonus-points: uint
  })

;; Input validation functions
(define-private (is-valid-principal (principal-addr principal))
  (not (is-eq principal-addr 'ST000000000000000000002AMW42H)))

(define-private (is-valid-uri (uri (string-ascii 256)))
  (let ((uri-len (len uri)))
    (and (>= uri-len MIN-URI-LENGTH) 
         (<= uri-len MAX-URI-LENGTH))))

(define-private (is-valid-streak-days (days uint))
  (and (> days u0) (<= days MAX-STREAK-DAYS)))

(define-private (is-valid-bonus-points (points uint))
  (<= points MAX-BONUS-POINTS))

;; Private utility functions
(define-private (is-contract-owner)
  (is-eq tx-sender CONTRACT-OWNER))

(define-private (get-next-token-id)
  (+ (var-get last-token-id) u1))

(define-private (custom-max (a uint) (b uint))
  (if (>= a b) a b))

(define-private (get-day-from-block (block-num uint))
  (/ block-num BLOCKS-PER-DAY))

(define-private (are-blocks-same-day (block1 uint) (block2 uint))
  (is-eq (get-day-from-block block1) (get-day-from-block block2)))

(define-private (are-blocks-consecutive-days (earlier-block uint) (later-block uint))
  (is-eq (get-day-from-block later-block) (+ (get-day-from-block earlier-block) u1)))

(define-private (get-user-data-or-default (user principal))
  (default-to 
    {
      active-streak-count: u0, 
      best-streak-record: u0, 
      last-activity-block: u0, 
      lifetime-check-ins: u0,
      account-creation-block: stacks-block-height
    }
    (map-get? user-habit-statistics user)))

(define-private (calculate-new-streak-count (user-data {active-streak-count: uint, best-streak-record: uint, last-activity-block: uint, lifetime-check-ins: uint, account-creation-block: uint}))
  (let ((last-block (get last-activity-block user-data))
        (current-block stacks-block-height))
    (if (is-eq last-block u0)
        u1 ;; First check-in
        (if (are-blocks-consecutive-days last-block current-block)
            (+ (get active-streak-count user-data) u1) ;; Continue streak
            u1)))) ;; Reset streak

;; Public functions

;; Initialize milestone rewards with input validation
(define-public (configure-milestone-reward (streak-days uint) (metadata-uri (string-ascii 256)) (bonus-points uint))
  (begin
    (asserts! (is-contract-owner) ERR-OWNER-ONLY)
    ;; Validate input parameters
    (asserts! (is-valid-streak-days streak-days) ERR-INVALID-STREAK-DAYS)
    (asserts! (is-valid-uri metadata-uri) ERR-INVALID-URI)
    (asserts! (is-valid-bonus-points bonus-points) ERR-INVALID-BONUS-POINTS)
    (ok (map-set streak-achievement-rewards 
                streak-days 
                {metadata-uri: metadata-uri, bonus-points: bonus-points}))))

;; Set contract metadata URI with validation
(define-public (set-contract-metadata-uri (uri (string-ascii 256)))
  (begin
    (asserts! (is-contract-owner) ERR-OWNER-ONLY)
    (asserts! (is-valid-uri uri) ERR-INVALID-URI)
    (ok (var-set contract-uri (some uri)))))

;; Toggle contract active state
(define-public (toggle-contract-state)
  (begin
    (asserts! (is-contract-owner) ERR-OWNER-ONLY)
    (ok (var-set contract-is-active (not (var-get contract-is-active))))))

;; Main check-in function
(define-public (perform-daily-checkin)
  (let ((current-user-data (get-user-data-or-default tx-sender))
        (current-block-height stacks-block-height))
    (begin
      ;; Validate contract is active
      (asserts! (var-get contract-is-active) ERR-UNAUTHORIZED-OPERATION)

      ;; Prevent duplicate check-ins on the same day
      (asserts! (not (are-blocks-same-day
                       (get last-activity-block current-user-data)
                       current-block-height))
                ERR-ALREADY-CHECKED-IN-TODAY)

      (let ((updated-streak-count (calculate-new-streak-count current-user-data))
            (updated-best-record (custom-max (get best-streak-record current-user-data)
                                           (calculate-new-streak-count current-user-data)))
            (updated-lifetime-total (+ (get lifetime-check-ins current-user-data) u1)))

        ;; Update user statistics
        (map-set user-habit-statistics tx-sender
          {
            active-streak-count: updated-streak-count,
            best-streak-record: updated-best-record,
            last-activity-block: current-block-height,
            lifetime-check-ins: updated-lifetime-total,
            account-creation-block: (get account-creation-block current-user-data)
          })

        ;; Attempt to mint milestone NFT if applicable
        (match (map-get? streak-achievement-rewards updated-streak-count)
          reward-data (mint-achievement-nft tx-sender updated-streak-count (get metadata-uri reward-data))
          (ok updated-streak-count))))))

;; Mint achievement NFT for milestones
(define-private (mint-achievement-nft (recipient principal) (streak-milestone uint) (metadata-uri (string-ascii 256)))
  (let ((new-token-id (get-next-token-id)))
    (begin
      (try! (nft-mint? streak-tracker-nft new-token-id recipient))
      (map-set token-metadata new-token-id metadata-uri)
      (var-set last-token-id new-token-id)
      (ok new-token-id))))

;; Transfer NFT ownership with validation
(define-public (transfer-nft (token-id uint) (sender principal) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender sender) ERR-NOT-TOKEN-OWNER)
    (asserts! (is-some (nft-get-owner? streak-tracker-nft token-id)) ERR-INVALID-TOKEN-ID)
    (asserts! (is-valid-principal recipient) ERR-INVALID-INPUT)
    (nft-transfer? streak-tracker-nft token-id sender recipient)))

;; Burn NFT (owner only)
(define-public (burn-nft (token-id uint))
  (let ((current-owner (unwrap! (nft-get-owner? streak-tracker-nft token-id) ERR-INVALID-TOKEN-ID)))
    (begin
      (asserts! (is-eq tx-sender current-owner) ERR-NOT-TOKEN-OWNER)
      (map-delete token-metadata token-id)
      (nft-burn? streak-tracker-nft token-id current-owner))))

;; Admin mint function with validation
(define-public (admin-mint-special (recipient principal) (metadata-uri (string-ascii 256)))
  (let ((new-token-id (get-next-token-id)))
    (begin
      (asserts! (is-contract-owner) ERR-OWNER-ONLY)
      (asserts! (is-valid-principal recipient) ERR-INVALID-INPUT)
      (asserts! (is-valid-uri metadata-uri) ERR-INVALID-URI)
      (try! (nft-mint? streak-tracker-nft new-token-id recipient))
      (map-set token-metadata new-token-id metadata-uri)
      (var-set last-token-id new-token-id)
      (ok new-token-id))))

;; Read-only functions

;; Get complete user statistics
(define-read-only (get-user-statistics (user principal))
  (map-get? user-habit-statistics user))

;; Get current active streak
(define-read-only (get-current-active-streak (user principal))
  (match (map-get? user-habit-statistics user)
    user-data (some (get active-streak-count user-data))
    none))

;; Get best streak record
(define-read-only (get-personal-best-streak (user principal))
  (match (map-get? user-habit-statistics user)
    user-data (some (get best-streak-record user-data))
    none))

;; Get lifetime check-ins
(define-read-only (get-lifetime-checkin-count (user principal))
  (match (map-get? user-habit-statistics user)
    user-data (some (get lifetime-check-ins user-data))
    none))

;; Check if user can perform check-in today
(define-read-only (can-checkin-today (user principal))
  (match (map-get? user-habit-statistics user)
    user-data (not (are-blocks-same-day (get last-activity-block user-data) stacks-block-height))
    true)) ;; New users can always check in

;; Get NFT owner
(define-read-only (get-nft-owner (token-id uint))
  (ok (nft-get-owner? streak-tracker-nft token-id)))

;; Get current token supply
(define-read-only (get-current-token-supply)
  (ok (var-get last-token-id)))

;; Get token metadata URI
(define-read-only (get-token-metadata-uri (token-id uint))
  (ok (map-get? token-metadata token-id)))

;; Get contract metadata URI
(define-read-only (get-contract-metadata-uri)
  (ok (var-get contract-uri)))

;; Get milestone reward information
(define-read-only (get-milestone-reward-info (streak-days uint))
  (map-get? streak-achievement-rewards streak-days))

;; Check if token exists
(define-read-only (does-token-exist (token-id uint))
  (is-some (nft-get-owner? streak-tracker-nft token-id)))

;; Get contract active status
(define-read-only (is-contract-active)
  (var-get contract-is-active))

;; SIP-009 NFT trait compliance functions
(define-read-only (get-token-uri (token-id uint))
  (ok (map-get? token-metadata token-id)))

(define-read-only (get-owner (token-id uint))
  (ok (nft-get-owner? streak-tracker-nft token-id)))

;; Initialize default milestone rewards
(map-set streak-achievement-rewards u7 
  {metadata-uri: "https://api.streaktracker.com/metadata/7-day-streak.json", bonus-points: u10})
(map-set streak-achievement-rewards u30 
  {metadata-uri: "https://api.streaktracker.com/metadata/30-day-streak.json", bonus-points: u50})
(map-set streak-achievement-rewards u100 
  {metadata-uri: "https://api.streaktracker.com/metadata/100-day-streak.json", bonus-points: u200})
(map-set streak-achievement-rewards u365 
  {metadata-uri: "https://api.streaktracker.com/metadata/365-day-streak.json", bonus-points: u1000})