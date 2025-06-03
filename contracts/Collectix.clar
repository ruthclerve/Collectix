(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_EXISTS (err u102))
(define-constant ERR_INSUFFICIENT_FUNDS (err u103))
(define-constant ERR_NOT_OWNER (err u104))
(define-constant ERR_INVALID_PRICE (err u105))
(define-constant ERR_SELF_TRANSFER (err u106))

(define-data-var next-collectible-id uint u1)
(define-data-var platform-fee-percentage uint u250)

(define-map collectibles
  { id: uint }
  {
    owner: principal,
    creator: principal,
    name: (string-ascii 64),
    description: (string-ascii 256),
    image-url: (string-ascii 256),
    category: (string-ascii 32),
    rarity: (string-ascii 16),
    price: uint,
    for-sale: bool,
    created-at: uint
  }
)

(define-map user-collectibles
  { owner: principal, collectible-id: uint }
  { owned: bool }
)

(define-map collectible-counts
  { owner: principal }
  { count: uint }
)

(define-map category-counts
  { category: (string-ascii 32) }
  { count: uint }
)

(define-map user-balances
  { user: principal }
  { balance: uint }
)

(define-public (create-collectible 
  (name (string-ascii 64))
  (description (string-ascii 256))
  (image-url (string-ascii 256))
  (category (string-ascii 32))
  (rarity (string-ascii 16))
  (price uint))
  (let
    (
      (collectible-id (var-get next-collectible-id))
      (creator tx-sender)
    )
    (asserts! (> price u0) ERR_INVALID_PRICE)
    (map-set collectibles
      { id: collectible-id }
      {
        owner: creator,
        creator: creator,
        name: name,
        description: description,
        image-url: image-url,
        category: category,
        rarity: rarity,
        price: price,
        for-sale: true,
        created-at: stacks-block-height
      }
    )
    (map-set user-collectibles
      { owner: creator, collectible-id: collectible-id }
      { owned: true }
    )
    (update-user-count creator 1)
    (update-category-count category 1)
    (var-set next-collectible-id (+ collectible-id u1))
    (ok collectible-id)
  )
)

(define-public (buy-collectible (collectible-id uint))
  (let
    (
      (collectible (unwrap! (map-get? collectibles { id: collectible-id }) ERR_NOT_FOUND))
      (buyer tx-sender)
      (seller (get owner collectible))
      (price (get price collectible))
      (platform-fee (/ (* price (var-get platform-fee-percentage)) u10000))
      (seller-amount (- price platform-fee))
      (buyer-balance (default-to u0 (get balance (map-get? user-balances { user: buyer }))))
    )
    (asserts! (get for-sale collectible) ERR_NOT_AUTHORIZED)
    (asserts! (not (is-eq buyer seller)) ERR_SELF_TRANSFER)
    (asserts! (>= buyer-balance price) ERR_INSUFFICIENT_FUNDS)
    
    (map-delete user-collectibles { owner: seller, collectible-id: collectible-id })
    (map-set user-collectibles
      { owner: buyer, collectible-id: collectible-id }
      { owned: true }
    )
    
    (map-set collectibles
      { id: collectible-id }
      (merge collectible { owner: buyer, for-sale: false })
    )
    
    (update-user-balance buyer (- buyer-balance price))
    (update-user-balance seller (+ (get-user-balance seller) seller-amount))
    
    (update-user-count seller (- 1))
    (update-user-count buyer 1)
    
    (ok true)
  )
)

(define-public (set-collectible-for-sale (collectible-id uint) (for-sale bool) (new-price uint))
  (let
    (
      (collectible (unwrap! (map-get? collectibles { id: collectible-id }) ERR_NOT_FOUND))
      (caller tx-sender)
    )
    (asserts! (is-eq caller (get owner collectible)) ERR_NOT_OWNER)
    (asserts! (or (not for-sale) (> new-price u0)) ERR_INVALID_PRICE)
    
    (map-set collectibles
      { id: collectible-id }
      (merge collectible { for-sale: for-sale, price: new-price })
    )
    (ok true)
  )
)

(define-public (transfer-collectible (collectible-id uint) (recipient principal))
  (let
    (
      (collectible (unwrap! (map-get? collectibles { id: collectible-id }) ERR_NOT_FOUND))
      (sender tx-sender)
    )
    (asserts! (is-eq sender (get owner collectible)) ERR_NOT_OWNER)
    (asserts! (not (is-eq sender recipient)) ERR_SELF_TRANSFER)
    
    (map-delete user-collectibles { owner: sender, collectible-id: collectible-id })
    (map-set user-collectibles
      { owner: recipient, collectible-id: collectible-id }
      { owned: true }
    )
    
    (map-set collectibles
      { id: collectible-id }
      (merge collectible { owner: recipient, for-sale: false })
    )
    
    (update-user-count sender (- 1))
    (update-user-count recipient 1)
    
    (ok true)
  )
)

(define-public (deposit-funds (amount uint))
  (let
    (
      (user tx-sender)
      (current-balance (get-user-balance user))
    )
    (update-user-balance user (+ current-balance amount))
    (ok true)
  )
)

(define-public (withdraw-funds (amount uint))
  (let
    (
      (user tx-sender)
      (current-balance (get-user-balance user))
    )
    (asserts! (>= current-balance amount) ERR_INSUFFICIENT_FUNDS)
    (update-user-balance user (- current-balance amount))
    (ok true)
  )
)

(define-public (set-platform-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (var-set platform-fee-percentage new-fee)
    (ok true)
  )
)

(define-read-only (get-collectible (collectible-id uint))
  (map-get? collectibles { id: collectible-id })
)

(define-read-only (get-user-collectible-count (user principal))
  (default-to u0 (get count (map-get? collectible-counts { owner: user })))
)

(define-read-only (get-category-count (category (string-ascii 32)))
  (default-to u0 (get count (map-get? category-counts { category: category })))
)

(define-read-only (get-user-balance (user principal))
  (default-to u0 (get balance (map-get? user-balances { user: user })))
)

(define-read-only (owns-collectible (user principal) (collectible-id uint))
  (default-to false (get owned (map-get? user-collectibles { owner: user, collectible-id: collectible-id })))
)

(define-read-only (get-next-collectible-id)
  (var-get next-collectible-id)
)

(define-read-only (get-platform-fee)
  (var-get platform-fee-percentage)
)

(define-private (update-user-count (user principal) (change int))
  (let
    (
      (current-count (get-user-collectible-count user))
      (new-count (if (> change 0)
                    (+ current-count (to-uint change))
                    (- current-count (to-uint (- change)))))
    )
    (map-set collectible-counts
      { owner: user }
      { count: new-count }
    )
  )
)

(define-private (update-category-count (category (string-ascii 32)) (change int))
  (let
    (
      (current-count (get-category-count category))
      (new-count (if (> change 0)
                    (+ current-count (to-uint change))
                    (- current-count (to-uint (- change)))))
    )
    (map-set category-counts
      { category: category }
      { count: new-count }
    )
  )
)

(define-private (update-user-balance (user principal) (new-balance uint))
  (map-set user-balances
    { user: user }
    { balance: new-balance }
  )
)