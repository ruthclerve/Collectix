# 🎨 Collectix - Virtual Collectibles Marketplace

> A decentralized marketplace for virtual collectibles including art, badges, cards, and lore built on Stacks blockchain! 🚀

## 📖 Overview

Collectix is a smart contract that enables users to create, buy, sell, and trade virtual collectibles in a decentralized marketplace. Each collectible is unique and can represent digital art, achievement badges, trading cards, or any other virtual item with lore and rarity.

## ✨ Features

- 🎭 **Create Collectibles**: Mint unique virtual items with metadata
- 💰 **Marketplace Trading**: Buy and sell collectibles with built-in escrow
- 🔄 **Transfer System**: Gift collectibles to other users
- 💳 **Wallet Integration**: Deposit and withdraw funds
- 📊 **Analytics**: Track ownership and category statistics
- 🏷️ **Categorization**: Organize collectibles by type and rarity
- 💸 **Platform Fees**: Configurable marketplace commission

## 🛠️ Core Functions

### Creating Collectibles
```clarity
(create-collectible name description image-url category rarity price)
```

### Trading
```clarity
(buy-collectible collectible-id)
(set-collectible-for-sale collectible-id for-sale new-price)
(transfer-collectible collectible-id recipient)
```

### Wallet Management
```clarity
(deposit-funds amount)
(withdraw-funds amount)
```

## 📋 Getting Started

### Prerequisites
- Clarinet CLI installed
- Stacks wallet for testing

### Installation

1. Clone the repository
```bash
git clone <your-repo>
cd collectix
```

2. Check contract syntax
```bash
clarinet check
```

3. Run tests
```bash
clarinet test
```

4. Deploy locally
```bash
clarinet console
```

## 🎮 Usage Examples

### Create Your First Collectible
```clarity
(contract-call? .Collectix create-collectible 
  "Dragon Card" 
  "A legendary fire-breathing dragon" 
  "https://example.com/dragon.png" 
  "cards" 
  "legendary" 
  u1000)
```

### Buy a Collectible
```clarity
;; First deposit funds
(contract-call? .Collectix deposit-funds u1000)

;; Then buy the collectible
(contract-call? .Collectix buy-collectible u1)
```

### Check Ownership
```clarity
(contract-call? .Collectix owns-collectible tx-sender u1)
```

## 🔍 Read-Only Functions

- `get-collectible` - Get collectible details
- `get-user-collectible-count` - Count user's collectibles
- `get-category-count` - Count collectibles in category
- `get-user-balance` - Check user's balance
- `owns-collectible` - Verify ownership
- `get-platform-fee` - Current marketplace fee

## 🏗️ Contract Architecture

The contract uses several data structures:
- **collectibles**: Main collectible data
- **user-collectibles**: Ownership mapping
- **collectible-counts**: User statistics
- **category-counts**: Category statistics
- **user-balances**: User wallet balances

## 🔐 Security Features

- Ownership verification for all transfers
- Balance checks before purchases
- Self-transfer prevention
- Authorization checks for admin functions
- Input validation for all parameters

## 🚀 Future Enhancements

- 🎲 Random collectible generation
- 🏆 Achievement system
- 📈 Price history tracking
- 🎪 Auction functionality
- 🎁 Collectible bundles
- ⭐ Rating and review system

## 📄 License

MIT License - feel free to build amazing things! 🌟

---

*Built with ❤️ on Stacks blockchain*


