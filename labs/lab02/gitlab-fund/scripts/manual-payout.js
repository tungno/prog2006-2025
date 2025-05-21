// This code can be used in a separate file that you'll run with Hardhat
// Save as scripts/manual-payout.js or similar

const { ethers } = require("hardhat");

async function main() {
    try {
        // Configuration - Replace these with your values
        const ISSUE_ID = 1; // The issue ID you're trying to process payout for
        const DEVELOPER_PAYOUTS_ADDRESS = "0xe7f1725E7734CE288F8367e1Bb143E90bb3F0512";
        const DEVELOPER_PRIVATE_KEY = "0x47e179ec197488593b187f80a00eb0da91f1b9d0b13f8733639f19c30a34926a"; // Developer's private key

        console.log("Starting manual payout process...");

        // Connect to local provider
        const provider = new ethers.providers.JsonRpcProvider("http://localhost:8545");

        // Create wallet from private key
        const developerWallet = new ethers.Wallet(DEVELOPER_PRIVATE_KEY, provider);
        console.log(`Using developer wallet: ${developerWallet.address}`);

        // Load the DeveloperPayouts contract
        const developerPayoutsAbi = [
            "function getClaimStatus(uint256 issueId) view returns (bool claimed, address developer, uint256 amount)",
            "function validatorMultiSig() view returns (address)",
            "function processPayout(uint256 issueId)"
        ];

        const developerPayouts = new ethers.Contract(
            DEVELOPER_PAYOUTS_ADDRESS,
            developerPayoutsAbi,
            developerWallet
        );

        // Check claim status
        const [claimed, developer, amount] = await developerPayouts.getClaimStatus(ISSUE_ID);
        console.log(`Claim status for issue #${ISSUE_ID}:`);
        console.log(`- Claimed: ${claimed}`);
        console.log(`- Developer: ${developer}`);
        console.log(`- Amount: ${ethers.utils.formatEther(amount)} ETH`);

        if (!claimed) {
            console.error("ERROR: Bounty not claimed yet");
            return;
        }

        if (developer.toLowerCase() !== developerWallet.address.toLowerCase()) {
            console.error("ERROR: You are not the developer who claimed this bounty");
            return;
        }

        // Get validator address
        const validatorAddress = await developerPayouts.validatorMultiSig();
        console.log(`Validator address: ${validatorAddress}`);

        // Load the ValidatorMultiSig contract
        const validatorAbi = [
            "function isApproved(uint256 issueId, address developer) view returns (bool)",
            "function getApprovalCount(uint256 issueId, address developer) view returns (uint256)",
            "function threshold() view returns (uint256)"
        ];

        const validator = new ethers.Contract(
            validatorAddress,
            validatorAbi,
            provider
        );

        // Check approvals
        const isApproved = await validator.isApproved(ISSUE_ID, developer);
        console.log(`Approved by validators: ${isApproved}`);

        if (!isApproved) {
            try {
                const approvalCount = await validator.getApprovalCount(ISSUE_ID, developer);
                let threshold;

                try {
                    threshold = await validator.threshold();
                } catch (e) {
                    try {
                        threshold = await validator.requiredApprovals();
                    } catch (e2) {
                        threshold = 2; // fallback
                    }
                }

                console.log(`Approval count: ${approvalCount} / ${threshold} required`);
            } catch (e) {
                console.error("Could not get approval details:", e.message);
            }

            console.error("ERROR: Not approved by validators yet");
            return;
        }

        // Try processing the payout with different gas settings
        console.log("Attempting to process payout...");

        const tx = await developerPayouts.processPayout(ISSUE_ID, {
            gasLimit: 500000,
            gasPrice: ethers.utils.parseUnits("50", "gwei") // Higher gas price
        });

        console.log(`Transaction sent: ${tx.hash}`);
        console.log("Waiting for confirmation...");

        const receipt = await tx.wait();
        console.log(`Transaction confirmed in block ${receipt.blockNumber}`);
        console.log(`Gas used: ${receipt.gasUsed.toString()}`);

        if (receipt.status === 1) {
            console.log("SUCCESS: Payout processed successfully!");
        } else {
            console.error("ERROR: Transaction reverted");
        }

    } catch (error) {
        console.error("ERROR:", error);

        // Provide more context for common errors
        if (error.message.includes("execution reverted")) {
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
    .catch((error) => {
        console.error(error);
        process.exit(1);
    });