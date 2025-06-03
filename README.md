# 🏠 Leasebit - Rent-to-Own Smart Contract

> 🚀 **Token-based rent-to-own agreements on the Stacks blockchain**

Leasebit enables property owners to create rent-to-own agreements where tenants can gradually acquire ownership through monthly payments. Built with Clarity smart contracts for transparency and security.

## ✨ Features

- 🏡 **Property Listing**: Landlords can list properties with rent-to-own terms
- 📝 **Smart Leases**: Automated lease agreements with built-in payment tracking
- 💰 **Progressive Ownership**: Tenants gain ownership after completing all payments
- 🔍 **Transparent Progress**: Real-time tracking of payment progress and ownership transfer
- ⚡ **Automated Transfers**: Ownership automatically transfers upon lease completion

## 🛠️ Core Functions

### For Property Owners

#### `create-property`
Create a new rent-to-own property listing
```clarity
(contract-call? .Leasebit create-property total-price monthly-rent lease-duration-months metadata)
```

#### `cancel-lease`
Cancel an active lease agreement
```clarity
(contract-call? .Leasebit cancel-lease lease-id)
```

### For Tenants

#### `start-lease`
Begin a rent-to-own agreement for a property
```clarity
(contract-call? .Leasebit start-lease property-id)
```

#### `make-payment`
Make monthly rent payment towards ownership
```clarity
(contract-call? .Leasebit make-payment lease-id)
```

### Read-Only Functions

#### `get-property`
Get property details
```clarity
(contract-call? .Leasebit get-property property-id)
```

#### `get-lease`
Get lease agreement details
```clarity
(contract-call? .Leasebit get-lease lease-id)
```

#### `get-lease-progress`
Check payment progress and completion percentage
```clarity
(contract-call? .Leasebit get-lease-progress lease-id)
```

## 🚀 Getting Started

### Prerequisites
- [Clarinet](https://github.com/hirosystems/clarinet) installed
- Stacks wallet with STX tokens

### Installation

```bash
clarinet new leasebit-project
```

```bash
cd leasebit-project
```

Copy the contract code to `contracts/Leasebit.clar`

```bash
clarinet check
```

```bash
clarinet test
```

### Usage Example

1. **Create a Property** (as landlord):
```clarity
(contract-call? .Leasebit create-property u1000000 u50000 u20 "Beautiful 2BR apartment downtown")
```

2. **Start a Lease** (as tenant):
```clarity
(contract-call? .Leasebit start-lease u1)
```

3. **Make Monthly Payments**:
```clarity
(contract-call? .Leasebit make-payment u1)
```

4. **Track Progress**:
```clarity
(contract-call? .Leasebit get-lease-progress u1)
```

## 📊 How It Works

1. 🏠 **Property Creation**: Landlord lists property with total price, monthly rent, and lease duration
2. 🤝 **Lease Initiation**: Tenant starts lease with first month's payment
3. 💳 **Monthly Payments**: Tenant makes regular payments tracked on-chain
4. 📈 **Progress Tracking**: Smart contract tracks payment progress automatically
5. 🎉 **Ownership Transfer**: Property ownership transfers to tenant upon completion

## 🔒 Security Features

- ✅ Payment validation and tracking
- ✅ Access control for lease operations
- ✅ Automatic ownership transfer
- ✅ Overdue payment detection
- ✅ Lease cancellation protection

## 🧪 Testing

```bash
clarinet test
```

```bash
clarinet console
```

## 📝 Error Codes

- `u100`: Unauthorized access
- `u101`: Invalid amount
- `u102`: Property not found
- `u103`: Lease not found
- `u104`: Payment overdue
- `u105`: Lease already completed
- `u106`: Insufficient payment
- `u107`: Lease already active
- `u108`: Not the tenant

## 🤝 Contributing

1. Fork the repository
2. Create your feature branch
3. Commit your changes
4. Push to the branch
5. Create a Pull Request


