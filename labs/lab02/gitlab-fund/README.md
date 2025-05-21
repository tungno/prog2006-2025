# GitLab-Fund: Decentralized Bounty System for GitLab Issues

GitLab-Fund is a decentralized application that enables funding and claiming bounties for GitLab issues through a secure, multi-signature validation system. This project demonstrates advanced Solidity concepts including contract interactions, event handling, and secure transaction processing.

## 📝 Project Overview

This system connects three key stakeholders:

1. **Funders**: Contribute ETH to fund GitLab issues they want to see resolved
2. **Developers**: Claim bounties by working on issues and submitting merge requests
3. **Validators**: Approve completed work through a multi-signature process before payouts

The system uses a 3-contract architecture to separate concerns:
- **FundManager.sol**: Handles depositing funds and allocating bounties to issues
- **DeveloperPayouts.sol**: Manages claim requests and processes payouts to developers
- **ValidatorMultiSig.sol**: Implements n-out-of-m multi-signature approval system

## 🚀 Getting Started

### Prerequisites

- [Node.js](https://nodejs.org/) (v18+ recommended)
- [Hardhat](https://hardhat.org/)
- [MetaMask](https://metamask.io/) for wallet interaction

### Installation

1. Clone the repository:
   ```bash
   git clone https://github.com/tungno/prog2006-2025.git
   cd labs/lab02/gitlab-fund
   ```

2. Install dependencies:
   ```bash
   npm install
   ```

3. Compile the contracts:
   ```bash
   npx hardhat typechain
   npx hardhat compile
   ```

## 🏗️ Running a Local Development Environment

1. Start a local Hardhat node in a terminal:
   ```bash
   npx hardhat node
   ```

2. Deploy all contracts (in a separate terminal):
   ```bash
   npx hardhat run scripts/deploy/03_deploy_payouts.ts --network localhost
   ```

3. Note the contract addresses from the output:
   ```
   FundManager deployed to: 0x5FbDB2315678afecb367f032d93F642f64180aa3
   ValidatorMultiSig deployed to: 0x9fE46736679d2D9a65F0992F2272dE9f3c7fa6e0
   DeveloperPayouts deployed to: 0xe7f1725E7734CE288F8367e1Bb143E90bb3F0512
   ```

4. Run post-deployment setup:
   ```bash
   npx hardhat run scripts/update-validator.ts --network localhost
   ```

## 💻 Running the Frontend

1. Start the frontend development server:
   ```bash
   cd frontend
   npm install
   npm run dev
   ```

2. Open your browser to the URL shown (typically http://localhost:5173)

## 🔄 Complete Workflow Demonstration

Follow these steps to experience the entire bounty lifecycle:

### Step 1: Fund Manager - Deposit & Allocate (as Funder)

1. Navigate to the Funder Dashboard
2. Connect your wallet:
   - Use Account #0: `0xac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80`
   - Contract: `0x5FbDB2315678afecb367f032d93F642f64180aa3`

3. Deposit 5 ETH:
   - Enter "5" in the Amount field
   - Click "Deposit"

4. Allocate a bounty:
   - Enter "1" for Issue #1
   - Enter "1" for 1 ETH bounty amount
   - Click "Allocate Bounty"

### Step 2: Developer Dashboard - Claim Bounty (as Developer)

1. Navigate to the Developer Dashboard
2. Connect your wallet:
   - Use Account #4: `0x47e179ec197488593b187f80a00eb0da91f1b9d0b13f8733639f19c30a34926a`
   - Contract: `0xe7f1725E7734CE288F8367e1Bb143E90bb3F0512`

3. Claim the bounty:
   - Enter "1" for Issue #1
   - Click "Claim Bounty"
   - Verify status shows "Claimed"

### Step 3: Validator Dashboard - First Approval (as Validator 1)

1. Navigate to the Validator Dashboard
2. Connect as first validator:
   - Use Account #1: `0x59c6995e998f97a5a0044966f0945389dc9e86dae88c7a8412f4603b6b78690d`
   - Contract: `0x9fE46736679d2D9a65F0992F2272dE9f3c7fa6e0`

3. Approve the payout:
   - Enter "1" for Issue #1
   - Enter developer address: `0x15d34AAf54267DB7D7c367839AAf71A00a2C6A65`
   - Click "Approve Payment"
   - Note approval count (1/2)

### Step 4: Validator Dashboard - Second Approval (as Validator 2)

1. Open a new browser tab or use incognito mode
2. Navigate to the Validator Dashboard
3. Connect as second validator:
   - Use Account #2: `0x5de4111afa1a4b94908f83103eb1f1706367c2e68ca870fc3fb9a804cdab365a`
   - Contract: `0x9fE46736679d2D9a65F0992F2272dE9f3c7fa6e0`

4. Approve the payout:
   - Enter "1" for Issue #1
   - Enter developer address: `0x15d34AAf54267DB7D7c367839AAf71A00a2C6A65`
   - Click "Approve Payment"
   - Note approval count (2/2)

### Step 5: Developer Dashboard - Process Payment (as Developer)

1. Return to the Developer Dashboard
2. Check and process your payment:
   - Enter "1" for Issue #1
   - Click "Check Claim Status" to refresh
   - Click "Process Payout"
   - Confirm transaction and see payout received

## 🛠️ Using Hardhat Tasks

For developers or advanced users, you can interact with the contracts using Hardhat tasks:

```bash
# Set environment variables
export FUND_MANAGER=0x5FbDB2315678afecb367f032d93F642f64180aa3
export DEVELOPER_PAYOUTS=0xe7f1725E7734CE288F8367e1Bb143E90bb3F0512
export VALIDATOR_MULTISIG=0x9fE46736679d2D9a65F0992F2272dE9f3c7fa6e0
export DEVELOPER=0x15d34AAf54267DB7D7c367839AAf71A00a2C6A65 # Account #4

# Deposit funds
npx hardhat deposit-funds --contract $FUND_MANAGER --amount 5

# Allocate bounty
npx hardhat allocate-bounty --contract $FUND_MANAGER --issueid 1 --amount 1

# Claim bounty
npx hardhat claim-bounty --contract $DEVELOPER_PAYOUTS --issueid 1

# Approve payout (run as validator)
npx hardhat approve-payout --contract $VALIDATOR_MULTISIG --issueid 1 --developer $DEVELOPER

# Check bounty status
npx hardhat get-bounty --fundmanager $FUND_MANAGER --payouts $DEVELOPER_PAYOUTS --issueid 1

# Process payout
npx hardhat process-payout --contract $DEVELOPER_PAYOUTS --issueid 1
```

## 🔍 Diagnostic Tool

For monitoring the system state:

1. Navigate to the Diagnostic component in the frontend
2. Enter the funder's address (Account #0): `0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266`
3. Enter an issue ID (e.g., "1")
4. Click "Check Status" to see the complete status of that bounty

## 📊 Contract Architecture

### FundManager
- Handles deposits from funders
- Manages bounty allocation to specific GitLab issues
- Tracks balances and allocations per funder
- Allows reallocation of funds between issues

### ValidatorMultiSig
- Implements n-out-of-m validation logic
- Tracks validator approvals for each bounty claim
- Manages validator additions and removals
- Provides threshold checking for payment approvals

### DeveloperPayouts
- Manages the bounty claiming process
- Verifies validator approvals before processing payments
- Enforces claim timeouts and validation rules
- Handles the actual ETH transfer to developers

## 🔒 Security Features

The system implements several security mechanisms:

1. **Reentrancy Protection**: All fund transfers are protected against reentrancy attacks
2. **Multi-Signature Validation**: Requires multiple validators to approve payments
3. **Timeouts**: Claims must be processed within 7 days
4. **Balance Checks**: Ensures sufficient funds before allocations
5. **Ownership Controls**: Only authorized users can perform sensitive operations

## 📄 License

This project is licensed under the MIT License.