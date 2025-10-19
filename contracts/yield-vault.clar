;; YieldVault - STX Vault with Reward Distribution

(define-map deposits
  { user: principal }
  { amount: uint, reward-debt: uint })

(define-data-var total-deposit uint u0)
(define-data-var acc-reward-per-share uint u0) ;; Scaled by 10^6 for precision

;; Deposit STX into the vault
(define-public (deposit)
  (let ((amount (stx-get-balance tx-sender)))
    (begin
      (asserts! (> amount u0) (err u100))

      (let (
            (existing (default-to { amount: u0, reward-debt: u0 } (map-get? deposits { user: tx-sender })))
            (total (var-get total-deposit))
            (acc-rps (var-get acc-reward-per-share))
            (new-amount (+ (get amount existing) amount))
            (reward-debt (/ (* new-amount acc-rps) u1000000))
          )

        (map-set deposits { user: tx-sender }
          { amount: new-amount, reward-debt: reward-debt })

        (var-set total-deposit (+ total amount))

        (ok { deposited: amount })
      )
    )))

;; Admin adds rewards to be shared by depositors
(define-public (distribute-reward)
  (let ((reward (stx-get-balance tx-sender)))
    (begin
      (asserts! (> reward u0) (err u101))
      (let ((total (var-get total-deposit)))
        (asserts! (> total u0) (err u102)) ;; No one to distribute to

        ;; Update acc-reward-per-share (scaled to 1e6)
        (var-set acc-reward-per-share
          (+ (var-get acc-reward-per-share)
             (/ (* reward u1000000) total)))

        (ok { distributed: reward })
      )
    )))

;; User claims their rewards
(define-public (claim)
  (let (
        (user-deposit (unwrap! (map-get? deposits { user: tx-sender }) (err u103)))
        (acc-rps (var-get acc-reward-per-share))
        (pending (- (/ (* (get amount user-deposit) acc-rps) u1000000)
                    (get reward-debt user-deposit)))
      )
    (begin
      (asserts! (> pending u0) (err u104))

      ;; Update reward debt
      (map-set deposits { user: tx-sender }
        { amount: (get amount user-deposit),
          reward-debt: (/ (* (get amount user-deposit) acc-rps) u1000000) })

      (try! (stx-transfer? pending (as-contract tx-sender) tx-sender))
      (ok { claimed: pending })
    )))

;; View user balance
(define-read-only (get-balance (user principal))
  (ok (map-get? deposits { user: user })))

;; View claimable reward
(define-read-only (get-pending-reward (user principal))
  (let (
        (user-deposit (default-to { amount: u0, reward-debt: u0 } (map-get? deposits { user: user })))
        (acc-rps (var-get acc-reward-per-share))
      )
    (ok (- (/ (* (get amount user-deposit) acc-rps) u1000000)
           (get reward-debt user-deposit)))))
