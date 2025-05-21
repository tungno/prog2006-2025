import { ethers } from "hardhat";

async function main() {
    // Configuration - update with your values
    const ISSUE_ID = 1;
    const DEVELOPER_PAYOUTS_ADDRESS = "0xe7f1725E7734CE288F8367e1Bb143E90bb3F0512";

    console.log("Starting payout process with enhanced debugging...");

    // Get developer account (change index as needed)
    const developer = (await ethers.getSigners())[4]; // Account #4
    console.log(`Using developer account: ${developer.address}`);

    // Get DeveloperPayouts contract
    const DeveloperPayouts = await ethers.getContractFactory("DeveloperPayouts");
    const developerPayouts = await DeveloperPayouts.attach(DEVELOPER_PAYOUTS_ADDRESS);

    console.log(`Checking claim status for issue #${ISSUE_ID}...`);

    // Check claim status first
    const [claimed, claimDeveloper, amount] = await developerPayouts.getClaimStatus(ISSUE_ID);
    console.log(`Claim status:`);
    console.log(`- Claimed: ${claimed}`);
    console.log(`- Developer: ${claimDeveloper}`);
    console.log(`- Amount: ${ethers.formatEther(amount)} ETH`);

    if (!claimed) {
        console.error("ERROR: Bounty not claimed yet");
        return;
    }

    // Get validator address and check it's valid
    const validatorAddress = await developerPayouts.validatorMultiSig();
    console.log(`Validator address: ${validatorAddress}`);

    if (validatorAddress === '0x0000000000000000000000000000000000000001') {
        console.error("ERROR: Invalid validator address. Please update the validator address first.");
        return;
    }

    // Check contract balance
    const contractBalance = await ethers.provider.getBalance(DEVELOPER_PAYOUTS_ADDRESS);
    console.log(`Contract balance: ${ethers.formatEther(contractBalance)} ETH`);

    if (contractBalance < amount) {
        console.error("WARNING: Contract balance is less than claim amount! Sending funds...");

        // Transfer ETH to the contract
        const [signer] = await ethers.getSigners();
        const tx = await signer.sendTransaction({
            to: DEVELOPER_PAYOUTS_ADDRESS,
            value: ethers.parseEther("2.0") // Transfer 2 ETH
        });
        await tx.wait();
        console.log(`Transferred 2 ETH to contract. TX: ${tx.hash}`);

        // Check updated balance
        const newBalance = await ethers.provider.getBalance(DEVELOPER_PAYOUTS_ADDRESS);
        console.log(`New contract balance: ${ethers.formatEther(newBalance)} ETH`);
    }

    console.log("\nAttempting to process payout...");

    try {
        // Use a higher gas limit
        const tx = await developerPayouts.processPayout(ISSUE_ID, {
            gasLimit: 500000
        });

        console.log(`Transaction sent: ${tx.hash}`);
        console.log("Waiting for confirmation...");

        const receipt = await tx.wait();
        console.log(`Transaction confirmed in block ${receipt.blockNumber}`);

        console.log("\nSUCCESS: Processed payout for issue #" + ISSUE_ID);
    } catch (error: any) {
        console.error("\nError processing payout:", error.message || error);

        // Provide more context for common errors
        if (error.message && error.message.includes("execution reverted")) {
            console.log("\nPossible reasons for reversion:");
            console.log("1. Bounty already paid out");
            console.log("2. Not enough validator approvals");
            console.log("3. Claim expired (more than 7 days old)");
            console.log("4. Contract doesn't have enough funds");
        }
    }
}

main()
    .then(() => process.exit(0))
    .catch(error => {
        console.error(error);
        process.exit(1);
    });