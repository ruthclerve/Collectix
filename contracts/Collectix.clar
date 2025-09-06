(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_EXISTS (err u102))
(define-constant ERR_INSUFFICIENT_FUNDS (err u103))
(define-constant ERR_NOT_OWNER (err u104))
(define-constant ERR_INVALID_PRICE (err u105))
(define-constant ERR_SELF_TRANSFER (err u106))
(define-constant ERR_AUCTION_NOT_FOUND (err u107))
(define-constant ERR_AUCTION_ENDED (err u108))
(define-constant ERR_AUCTION_ACTIVE (err u109))
(define-constant ERR_BID_TOO_LOW (err u110))
(define-constant ERR_NOT_HIGHEST_BIDDER (err u111))
(define-constant ERR_AUCTION_NOT_ENDED (err u112))
(define-constant ERR_BUNDLE_NOT_FOUND (err u113))
(define-constant ERR_BUNDLE_EMPTY (err u114))
(define-constant ERR_BUNDLE_TOO_LARGE (err u115))
(define-constant ERR_COLLECTIBLE_IN_BUNDLE (err u116))
(define-constant ERR_BUNDLE_NOT_FOR_SALE (err u117))

(define-data-var next-collectible-id uint u1)
(define-data-var platform-fee-percentage uint u250)
(define-data-var next-auction-id uint u1)
(define-data-var next-bundle-id uint u1)

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

(define-map auctions
  { id: uint }
  {
    collectible-id: uint,
    seller: principal,
    starting-price: uint,
    current-price: uint,
    highest-bidder: (optional principal),
    end-block: uint,
    active: bool,
    created-at: uint
  }
)

(define-map auction-bids
  { auction-id: uint, bidder: principal }
  { amount: uint, block-height: uint }
)

(define-map user-auction-counts
  { user: principal }
  { count: uint }
)

(define-map bundles
  { id: uint }
  {
    creator: principal,
    name: (string-ascii 64),
    description: (string-ascii 256),
    collectible-ids: (list 10 uint),
    total-price: uint,
    for-sale: bool,
    created-at: uint
  }
)

(define-map bundle-collectibles
  { bundle-id: uint, collectible-id: uint }
  { included: bool }
)

(define-map user-bundle-counts
  { user: principal }
  { count: uint }
)

(define-public (create-bundle 
  (name (string-ascii 64))
  (description (string-ascii 256))
  (collectible-ids (list 10 uint))
  (total-price uint))
  (let
    (
      (bundle-id (var-get next-bundle-id))
      (creator tx-sender)
      (collectible-count (len collectible-ids))
    )
    (asserts! (> collectible-count u0) ERR_BUNDLE_EMPTY)
    (asserts! (<= collectible-count u10) ERR_BUNDLE_TOO_LARGE)
    (asserts! (> total-price u0) ERR_INVALID_PRICE)
    (try! (validate-bundle-ownership creator collectible-ids))
    
    (map-set bundles
      { id: bundle-id }
      {
        creator: creator,
        name: name,
        description: description,
        collectible-ids: collectible-ids,
        total-price: total-price,
        for-sale: true,
        created-at: stacks-block-height
      }
    )
    
    (try! (mark-collectibles-bundled bundle-id collectible-ids))
    (update-user-bundle-count creator 1)
    (var-set next-bundle-id (+ bundle-id u1))
    (ok bundle-id)
  )
)

(define-public (buy-bundle (bundle-id uint))
  (let
    (
      (bundle (unwrap! (map-get? bundles { id: bundle-id }) ERR_BUNDLE_NOT_FOUND))
      (buyer tx-sender)
      (seller (get creator bundle))
      (price (get total-price bundle))
      (platform-fee (/ (* price (var-get platform-fee-percentage)) u10000))
      (seller-amount (- price platform-fee))
      (buyer-balance (get-user-balance buyer))
      (collectible-ids (get collectible-ids bundle))
    )
    (asserts! (get for-sale bundle) ERR_BUNDLE_NOT_FOR_SALE)
    (asserts! (not (is-eq buyer seller)) ERR_SELF_TRANSFER)
    (asserts! (>= buyer-balance price) ERR_INSUFFICIENT_FUNDS)
    
    (try! (execute-bundle-transfers seller buyer collectible-ids))
    (try! (unmark-collectibles-bundled bundle-id collectible-ids))
    
    (map-set bundles
      { id: bundle-id }
      (merge bundle { creator: buyer, for-sale: false })
    )
    
    (update-user-balance buyer (- buyer-balance price))
    (update-user-balance seller (+ (get-user-balance seller) seller-amount))
    
    (update-user-bundle-count seller (- 1))
    (update-user-bundle-count buyer 1)
    
    (ok true)
  )
)

(define-public (disband-bundle (bundle-id uint))
  (let
    (
      (bundle (unwrap! (map-get? bundles { id: bundle-id }) ERR_BUNDLE_NOT_FOUND))
      (caller tx-sender)
      (collectible-ids (get collectible-ids bundle))
    )
    (asserts! (is-eq caller (get creator bundle)) ERR_NOT_OWNER)
    
    (try! (unmark-collectibles-bundled bundle-id collectible-ids))
    (map-delete bundles { id: bundle-id })
    (update-user-bundle-count caller (- 1))
    (ok true)
  )
)

(define-public (set-bundle-for-sale (bundle-id uint) (for-sale bool) (new-price uint))
  (let
    (
      (bundle (unwrap! (map-get? bundles { id: bundle-id }) ERR_BUNDLE_NOT_FOUND))
      (caller tx-sender)
    )
    (asserts! (is-eq caller (get creator bundle)) ERR_NOT_OWNER)
    (asserts! (or (not for-sale) (> new-price u0)) ERR_INVALID_PRICE)
    
    (map-set bundles
      { id: bundle-id }
      (merge bundle { for-sale: for-sale, total-price: new-price })
    )
    (ok true)
  )
)

(define-read-only (get-bundle (bundle-id uint))
  (map-get? bundles { id: bundle-id })
)

(define-read-only (get-user-bundle-count (user principal))
  (default-to u0 (get count (map-get? user-bundle-counts { user: user })))
)

(define-read-only (get-next-bundle-id)
  (var-get next-bundle-id)
)

(define-read-only (is-collectible-in-bundle (collectible-id uint))
  (default-to false (get included (map-get? bundle-collectibles { bundle-id: u1, collectible-id: collectible-id })))
)

(define-private (validate-bundle-ownership (owner principal) (collectible-ids (list 10 uint)))
  (fold check-collectible-ownership collectible-ids (ok true))
)

(define-private (check-collectible-ownership (collectible-id uint) (prev-result (response bool uint)))
  (match prev-result
    success
    (if (owns-collectible tx-sender collectible-id)
      (ok true)
      ERR_NOT_OWNER)
    error
    (err error)
  )
)

(define-private (mark-collectibles-bundled (bundle-id uint) (collectible-ids (list 10 uint)))
  (fold mark-collectible-bundled collectible-ids (ok bundle-id))
)

(define-private (mark-collectible-bundled (collectible-id uint) (prev-result (response uint uint)))
  (match prev-result
    success
    (begin
      (map-set bundle-collectibles
        { bundle-id: success, collectible-id: collectible-id }
        { included: true }
      )
      (ok success)
    )
    error
    (err error)
  )
)

(define-private (unmark-collectibles-bundled (bundle-id uint) (collectible-ids (list 10 uint)))
  (fold unmark-collectible-bundled collectible-ids (ok bundle-id))
)

(define-private (unmark-collectible-bundled (collectible-id uint) (prev-result (response uint uint)))
  (match prev-result
    success
    (begin
      (map-delete bundle-collectibles { bundle-id: success, collectible-id: collectible-id })
      (ok success)
    )
    error
    (err error)
  )
)

(define-private (execute-bundle-transfers (from principal) (to principal) (collectible-ids (list 10 uint)))
  (fold process-collectible-transfer collectible-ids (ok { from: from, to: to }))
)

(define-private (process-collectible-transfer (collectible-id uint) (prev-result (response { from: principal, to: principal } uint)))
  (match prev-result
    success
    (begin
      (transfer-collectible-ownership collectible-id (get from success) (get to success))
      (ok success)
    )
    error
    (err error)
  )
)

(define-private (update-user-bundle-count (user principal) (change int))
  (let
    (
      (current-count (get-user-bundle-count user))
      (new-count (if (> change 0)
                    (+ current-count (to-uint change))
                    (- current-count (to-uint (- change)))))
    )
    (map-set user-bundle-counts
      { user: user }
      { count: new-count }
    )
  )
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

(define-public (create-auction (collectible-id uint) (starting-price uint) (duration-blocks uint))
  (let
    (
      (collectible (unwrap! (map-get? collectibles { id: collectible-id }) ERR_NOT_FOUND))
      (auction-id (var-get next-auction-id))
      (seller tx-sender)
      (end-block (+ stacks-block-height duration-blocks))
    )
    (asserts! (is-eq seller (get owner collectible)) ERR_NOT_OWNER)
    (asserts! (> starting-price u0) ERR_INVALID_PRICE)
    (asserts! (> duration-blocks u0) ERR_INVALID_PRICE)
    (asserts! (not (get for-sale collectible)) ERR_AUCTION_ACTIVE)
    
    (map-set auctions
      { id: auction-id }
      {
        collectible-id: collectible-id,
        seller: seller,
        starting-price: starting-price,
        current-price: starting-price,
        highest-bidder: none,
        end-block: end-block,
        active: true,
        created-at: stacks-block-height
      }
    )
    
    (map-set collectibles
      { id: collectible-id }
      (merge collectible { for-sale: false })
    )
    
    (update-user-auction-count seller 1)
    (var-set next-auction-id (+ auction-id u1))
    (ok auction-id)
  )
)

(define-public (place-bid (auction-id uint) (bid-amount uint))
  (let
    (
      (auction (unwrap! (map-get? auctions { id: auction-id }) ERR_AUCTION_NOT_FOUND))
      (bidder tx-sender)
      (current-price (get current-price auction))
      (bidder-balance (get-user-balance bidder))
      (seller (get seller auction))
      (end-block (get end-block auction))
    )
    (asserts! (get active auction) ERR_AUCTION_ENDED)
    (asserts! (<= stacks-block-height end-block) ERR_AUCTION_ENDED)
    (asserts! (not (is-eq bidder seller)) ERR_SELF_TRANSFER)
    (asserts! (> bid-amount current-price) ERR_BID_TOO_LOW)
    (asserts! (>= bidder-balance bid-amount) ERR_INSUFFICIENT_FUNDS)
    
    (match (get highest-bidder auction)
      previous-bidder
      (begin
        (update-user-balance previous-bidder (+ (get-user-balance previous-bidder) current-price))
        true
      )
      true
    )
    
    (update-user-balance bidder (- bidder-balance bid-amount))
    
    (map-set auctions
      { id: auction-id }
      (merge auction { 
        current-price: bid-amount, 
        highest-bidder: (some bidder) 
      })
    )
    
    (map-set auction-bids
      { auction-id: auction-id, bidder: bidder }
      { amount: bid-amount, block-height: stacks-block-height }
    )
    
    (ok true)
  )
)

(define-public (end-auction (auction-id uint))
  (let
    (
      (auction (unwrap! (map-get? auctions { id: auction-id }) ERR_AUCTION_NOT_FOUND))
      (caller tx-sender)
      (seller (get seller auction))
      (collectible-id (get collectible-id auction))
      (current-price (get current-price auction))
      (end-block (get end-block auction))
      (platform-fee (/ (* current-price (var-get platform-fee-percentage)) u10000))
      (seller-amount (- current-price platform-fee))
    )
    (asserts! (get active auction) ERR_AUCTION_ENDED)
    (asserts! (> stacks-block-height end-block) ERR_AUCTION_NOT_ENDED)
    
    (map-set auctions
      { id: auction-id }
      (merge auction { active: false })
    )
    
    (match (get highest-bidder auction)
      winner
      (begin
        (transfer-collectible-ownership collectible-id seller winner)
        (update-user-balance seller (+ (get-user-balance seller) seller-amount))
        (update-user-auction-count seller (- 1))
        (ok (some winner))
      )
      (begin
        (let
          (
            (collectible (unwrap! (map-get? collectibles { id: collectible-id }) ERR_NOT_FOUND))
          )
          (map-set collectibles
            { id: collectible-id }
            (merge collectible { for-sale: true })
          )
        )
        (update-user-auction-count seller (- 1))
        (ok none)
      )
    )
  )
)

(define-public (cancel-auction (auction-id uint))
  (let
    (
      (auction (unwrap! (map-get? auctions { id: auction-id }) ERR_AUCTION_NOT_FOUND))
      (caller tx-sender)
      (seller (get seller auction))
      (collectible-id (get collectible-id auction))
      (current-price (get current-price auction))
    )
    (asserts! (is-eq caller seller) ERR_NOT_OWNER)
    (asserts! (get active auction) ERR_AUCTION_ENDED)
    (asserts! (is-none (get highest-bidder auction)) ERR_AUCTION_ACTIVE)
    
    (map-set auctions
      { id: auction-id }
      (merge auction { active: false })
    )
    
    (let
      (
        (collectible (unwrap! (map-get? collectibles { id: collectible-id }) ERR_NOT_FOUND))
      )
      (map-set collectibles
        { id: collectible-id }
        (merge collectible { for-sale: true })
      )
    )
    
    (update-user-auction-count seller (- 1))
    (ok true)
  )
)

(define-read-only (get-auction (auction-id uint))
  (map-get? auctions { id: auction-id })
)

(define-read-only (get-auction-bid (auction-id uint) (bidder principal))
  (map-get? auction-bids { auction-id: auction-id, bidder: bidder })
)

(define-read-only (get-user-auction-count (user principal))
  (default-to u0 (get count (map-get? user-auction-counts { user: user })))
)

(define-read-only (get-next-auction-id)
  (var-get next-auction-id)
)

(define-read-only (is-auction-active (auction-id uint))
  (match (map-get? auctions { id: auction-id })
    auction (and (get active auction) (<= stacks-block-height (get end-block auction)))
    false
  )
)

(define-read-only (get-auction-time-remaining (auction-id uint))
  (match (map-get? auctions { id: auction-id })
    auction
    (if (<= stacks-block-height (get end-block auction))
      (some (- (get end-block auction) stacks-block-height))
      (some u0)
    )
    none
  )
)

(define-private (transfer-collectible-ownership (collectible-id uint) (from principal) (to principal))
  (let
    (
      (collectible (unwrap-panic (map-get? collectibles { id: collectible-id })))
    )
    (map-delete user-collectibles { owner: from, collectible-id: collectible-id })
    (map-set user-collectibles
      { owner: to, collectible-id: collectible-id }
      { owned: true }
    )
    
    (map-set collectibles
      { id: collectible-id }
      (merge collectible { owner: to, for-sale: false })
    )
    
    (update-user-count from (- 1))
    (update-user-count to 1)
  )
)

(define-private (update-user-auction-count (user principal) (change int))
  (let
    (
      (current-count (get-user-auction-count user))
      (new-count (if (> change 0)
                    (+ current-count (to-uint change))
                    (- current-count (to-uint (- change)))))
    )
    (map-set user-auction-counts
      { user: user }
      { count: new-count }
    )
  )
)




