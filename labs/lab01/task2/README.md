# Rock Paper Scissors DApp - Complete Tutorial

This tutorial provides detailed instructions on how to set up, deploy, and play the Rock Paper Scissors game on the Ethereum blockchain (Sepolia testnet).

## Game Overview

This is a decentralized Rock-Paper-Scissors game where two players can compete and bet ETH on the outcome. The game works as follows:

- **Paper (P)** beats **Rock (R)**
- **Rock (R)** beats **Scissors (S)**
- **Scissors (S)** beats **Paper (P)**

## Game Protocol

The game consists of several phases to ensure fair play on the blockchain:

1. **Commit Phase**: Players "commit" to their move (R, P, or S) by submitting a hash of their move plus a secret "salt" value
2. **Reveal Phase**: After both players have committed, they reveal their actual moves by providing the original move and salt
3. **Resolution**: The smart contract determines the winner based on the game rules
4. **Withdrawal**: The winner can withdraw the entire pot (2x the bet amount). In case of a tie, each player gets their bet back.

## Prerequisites

- [MetaMask](https://metamask.io/) browser extension installed
- Some Sepolia testnet ETH (get from [Sepolia faucet](https://sepoliafaucet.com/))
- Node.js and npm installed

## Setup Guide

### 1. Clone/Download the Project

If you're using git:
```bash
git clone https://github.com/tungno/prog2006-2025.git
cd labs/lab01/task2
```

### 2. Install Dependencies

```bash
npm install
```

### 3. Compile the Smart Contract

```bash
npx hardhat compile
```

### 4. Deploy to Sepolia Testnet

You'll need to set up your `.env` file with your private key and Infura/Alchemy API key. Create a file named `.env` with the following content:

```
PRIVATE_KEY=your_metamask_private_key
INFURA_API_KEY=your_infura_api_key
```

Then deploy the contract:

```bash
npx hardhat run scripts/deploy.js --network sepolia
```

Note the deployed contract address output from this command.

### 5. Configure the Frontend

Update the contract address in `frontend/app.js`:

```javascript
// In app.js, around line 2:
const contractAddress = "YOUR_DEPLOYED_CONTRACT_ADDRESS";
```

Also, make sure the contract ABI is correctly linked:

```bash
# Copy the compiled artifact to the frontend folder
cp artifacts/contracts/RockPaperScissors.sol/RockPaperScissors.json frontend/
```

### 6. Start the Frontend

You can use any static file server. Here are two options:

**Option 1: Using VS Code Live Server**
- Open the project in VS Code
- Right-click on `frontend/index.html` and select "Open with Live Server"

**Option 2: Using http-server**
```bash
npm install -g http-server
cd frontend
http-server
```

Then open your browser to the URL shown (typically http://localhost:8080).

## Playing the Game (Step-by-Step)

### ▶️ Player 1

#### Step 1: Connect Your Wallet
1. Open the DApp in your browser
2. Click "Connect Wallet" button
3. Approve the MetaMask connection

#### Step 2: Generate a Hashed Move
1. In the "Generate Hashed Move" section, enter:
   - Your choice (`R` for Rock, `P` for Paper, or `S` for Scissors)
   - A secret "salt" value (like "mysecret123")
2. Click "Generate Hash"
3. The hash will be displayed and automatically populated in the "Play" section

#### Step 3: Submit Your Move
1. Verify the hash in the "Play" section
2. Enter your stake amount (e.g., 0.001 ETH) in the "Stake" field
3. Click "Play" button
4. Approve the transaction in MetaMask and wait for confirmation
5. Share the DApp link with player 2

### ▶️ Player 2

Player 2 follows the same steps as Player 1 (Connect, Generate Hash, Play).

### ▶️ Revealing Moves (Both Players)

After both players have committed their moves, the reveal phase begins:

1. In the "Reveal" section, enter:
   - Your original move (R, P, or S)
   - The exact same salt value you used during the commit phase
2. Click "Reveal" button
3. Approve the transaction in MetaMask and wait for confirmation

### ▶️ Collecting Winnings

After both players have revealed their moves:

1. Click "Withdraw" button
2. If you won or it was a tie, you'll be able to withdraw your winnings
3. Approve the transaction in MetaMask and wait for confirmation

## Important Game Rules

- You must reveal your move within 5 minutes after both players have committed
- If only one player reveals their move within the time limit, they can claim the entire pot
- Each match requires exactly two different players
- Both players must bet the same amount of ETH
- The contract is designed to be completely fair and tamper-proof

## Troubleshooting

- **"Error: Contract not initialized"**: Ensure you have connected your wallet using the "Connect Wallet" button
- **"Error: Cannot play against yourself"**: You cannot play both sides of the game from the same Ethereum address
- **"Error: Game already has two players"**: Wait for the current game to complete before starting a new one
- **"Error: Stake must match the first player's stake"**: Player 2 must bet the exact same amount as Player 1
- **"Error: Invalid reveal"**: Ensure you're using the exact move and salt that you committed with

## Gas Costs

- **Play function**: Approximately 100,000-150,000 gas
- **Reveal function**: Approximately 50,000-70,000 gas
- **Withdraw function**: Approximately 30,000-50,000 gas

## Technical Details

The smart contract uses a commit-reveal pattern to ensure no one can cheat by seeing the other player's move. Your move is hashed with a secret salt value, and this hash is stored on the blockchain. Only when both players have committed their moves do they reveal what they actually played, providing both their move and salt to verify against the stored hash.

The contract contains security measures including:
- Prevention of re-entrancy attacks
- No self-play allowed
- Timeout mechanisms to prevent user funds from being locked
- Equal stakes for both players

## Game Mechanics

1. **Committing**: The hash is calculated as `keccak256(abi.encodePacked(move, salt))` where `move` is one of "R", "P", or "S" and `salt` is a random string
2. **Revealing**: Players reveal by submitting their original move and salt, which the contract verifies by recomputing the hash
3. **Winner Determination**: The contract compares the moves using traditional Rock-Paper-Scissors rules to determine the winner
4. **Withdrawal**: Players call the withdraw function to collect their winnings (or refund in case of a tie)

Enjoy the game and may the best player win!