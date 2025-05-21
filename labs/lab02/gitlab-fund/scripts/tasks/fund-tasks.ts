import { task } from "hardhat/config";
import { ethers } from "hardhat";
import type { FundManager, DeveloperPayouts, ValidatorMultiSig } from "../../typechain-types";
import { HardhatRuntimeEnvironment } from "hardhat/types";

// Task to deposit funds
task("deposit-funds", "Deposits funds to the FundManager")
    .addParam("contract", "The FundManager contract address")
    .addParam("amount", "Amount to deposit in ETH")
    .setAction(async (taskArgs, hre) => {
        const signer = (await hre.ethers.getSigners())[0];
        const FundManager = await hre.ethers.getContractFactory("FundManager");
        const fundManager = FundManager.attach(taskArgs.contract) as unknown as FundManager;

        const amount = hre.ethers.parseEther(taskArgs.amount);
        try {
            const tx = await fundManager.connect(signer).depositFunds({ value: amount });
            console.log("Transaction hash:", tx.hash);
            await tx.wait();
            console.log(`Successfully deposited ${taskArgs.amount} ETH to FundManager`);
        } catch (error) {
            console.error("Error depositing funds:", error);
        }
    });

// Task to allocate bounty
task("allocate-bounty", "Allocates a bounty to an issue")
    .addParam("contract", "The FundManager contract address")
    .addParam("issueid", "GitLab issue ID")
    .addParam("amount", "Bounty amount in ETH")
    .setAction(async (taskArgs, hre) => {
        const signer = (await hre.ethers.getSigners())[0];
        const FundManager = await hre.ethers.getContractFactory("FundManager");
        const fundManager = FundManager.attach(taskArgs.contract) as unknown as FundManager;

        const amount = hre.ethers.parseEther(taskArgs.amount);
        try {
            const tx = await fundManager.connect(signer).allocateBounty(taskArgs.issueid, amount);
            console.log("Transaction hash:", tx.hash);
            await tx.wait();
            console.log(`Successfully allocated ${taskArgs.amount} ETH to issue #${taskArgs.issueid}`);
        } catch (error) {
            console.error("Error allocating bounty:", error);
        }
    });

// Task to approve payout
task("approve-payout", "Approves a payout as a validator")
    .addParam("contract", "The ValidatorMultiSig contract address")
    .addParam("issueid", "GitLab issue ID")
    .addParam("developer", "Developer address")
    .setAction(async (taskArgs, hre) => {
        const signer = (await hre.ethers.getSigners())[0];
        const ValidatorMultiSig = await hre.ethers.getContractFactory("ValidatorMultiSig");
        const validatorMultiSig = ValidatorMultiSig.attach(taskArgs.contract) as unknown as ValidatorMultiSig;

        try {
            const tx = await validatorMultiSig.connect(signer).approvePayment(taskArgs.issueid, taskArgs.developer);
            await tx.wait();
            console.log(`Approved payout for issue #${taskArgs.issueid} to ${taskArgs.developer}`);
        } catch (error) {
            console.error("Error approving payout:", error);
        }
    });

// Task to claim bounty
task("claim-bounty", "Claims a bounty for an issue")
    .addParam("contract", "The DeveloperPayouts contract address")
    .addParam("issueid", "GitLab issue ID")
    .setAction(async (taskArgs, hre) => {
        const signer = (await hre.ethers.getSigners())[0];
        const DeveloperPayouts = await hre.ethers.getContractFactory("DeveloperPayouts");
        const developerPayouts = DeveloperPayouts.attach(taskArgs.contract) as unknown as DeveloperPayouts;

        try {
            const tx = await developerPayouts.connect(signer).claimBounty(taskArgs.issueid);
            await tx.wait();
            console.log(`Claimed bounty for issue #${taskArgs.issueid}`);
        } catch (error) {
            console.error("Error claiming bounty:", error);
        }
    });

// Task to get bounty status
task("get-bounty", "Gets bounty information for an issue")
    .addParam("fundmanager", "The FundManager contract address")
    .addParam("payouts", "The DeveloperPayouts contract address")
    .addParam("issueid", "GitLab issue ID")
    .setAction(async (taskArgs, hre) => {
        try {
            console.log("Checking contracts...");
            console.log(`FundManager address: ${taskArgs.fundmanager}`);
            console.log(`DeveloperPayouts address: ${taskArgs.payouts}`);
            console.log(`Issue ID: ${taskArgs.issueid}`);

            const FundManager = await hre.ethers.getContractFactory("FundManager");
            const fundManager = FundManager.attach(taskArgs.fundmanager);

            const DeveloperPayouts = await hre.ethers.getContractFactory("DeveloperPayouts");
            const developerPayouts = DeveloperPayouts.attach(taskArgs.payouts);

            console.log("\nFetching bounty details...");

            // Try to get basic bounty amount first
            const bountyAmount = await fundManager.getIssueBounty(taskArgs.issueid);
            console.log(`Basic bounty amount: ${hre.ethers.formatEther(bountyAmount)} ETH`);

            // Get full bounty details
            const [amount, funder, createdAt, active] = await fundManager.getBountyDetails(taskArgs.issueid);

            console.log("\nBounty Details:");
            console.log("------------------------");
            console.log(`Amount: ${hre.ethers.formatEther(amount)} ETH`);
            console.log(`Funder: ${funder}`);
            console.log(`Created: ${new Date(Number(createdAt) * 1000).toLocaleString()}`);
            console.log(`Active: ${active}`);

            // Get claim details
            const [claimed, developer, claimAmount] = await developerPayouts.getClaimStatus(taskArgs.issueid);

            console.log("\nClaim Status:");
            console.log("------------------------");
            console.log(`Claimed: ${claimed}`);
            console.log(`Developer: ${developer}`);
            console.log(`Claim Amount: ${hre.ethers.formatEther(claimAmount)} ETH`);

        } catch (error) {
            console.error("\nError Details:");
            console.error("------------------------");
            if ((error as any).code === 'BAD_DATA') {
                console.error("Contract returned invalid data. This usually means:");
                console.error("1. The contract addresses are incorrect");
                console.error("2. The contracts have been redeployed");
                console.error("3. The contract state is not as expected");
            } else {
                console.error("Unexpected error:", error);
            }
            console.error("\nTry redeploying the contracts and updating your environment variables.");
        }
    });

// Process payout task
task("process-payout", "Process the payout for a claimed bounty")
    .addParam("contract", "The DeveloperPayouts contract address")
    .addParam("issueid", "GitLab issue ID")
    .setAction(async (taskArgs, hre) => {
        const signer = (await hre.ethers.getSigners())[0];
        const DeveloperPayouts = await hre.ethers.getContractFactory("DeveloperPayouts");
        const developerPayouts = DeveloperPayouts.attach(taskArgs.contract) as unknown as DeveloperPayouts;

        try {
            const tx = await developerPayouts.connect(signer).processPayout(taskArgs.issueid);
            await tx.wait();
            console.log(`Processed payout for issue #${taskArgs.issueid}`);
        } catch (error) {
            console.error("Error processing payout:", error);
        }
    });

// Simplified process payout task with extra parameters
task("process-payout-fix", "Process the payout for a claimed bounty with extra debugging")
    .addParam("contract", "The DeveloperPayouts contract address")
    .addParam("issueid", "GitLab issue ID")
    .setAction(async (taskArgs, hre: HardhatRuntimeEnvironment) => {
        const signer = (await hre.ethers.getSigners())[0];
        console.log(`Using signer: ${signer.address}`);

        const DeveloperPayouts = await hre.ethers.getContractFactory("DeveloperPayouts");
        const developerPayouts = DeveloperPayouts.attach(taskArgs.contract) as unknown as DeveloperPayouts;

        console.log(`Checking claim status for issue #${taskArgs.issueid}...`);

        // Check claim status first
        try {
            const [claimed, developer, amount] = await developerPayouts.getClaimStatus(
                taskArgs.issueid
            );

            console.log(`Claim status:`);
            console.log(`- Claimed: ${claimed}`);
            console.log(`- Developer: ${developer}`);
            console.log(`- Amount: ${hre.ethers.formatEther(amount)} ETH`);

            if (!claimed) {
                console.error("ERROR: Bounty not claimed yet");
                return;
            }

            // Get validator address
            const validatorAddress = await developerPayouts.validatorMultiSig();
            console.log(`Validator address: ${validatorAddress}`);

            // Check if it's a valid address (not 0x0000...0001)
            if (validatorAddress === '0x0000000000000000000000000000000000000001') {
                console.error("ERROR: Invalid validator address. Please update the validator address first.");
                console.error("Run the update-validator.ts script to fix this issue.");
                return;
            }

            // Check contract balance
            const contractBalance = await hre.ethers.provider.getBalance(taskArgs.contract);
            console.log(`Contract balance: ${hre.ethers.formatEther(contractBalance)} ETH`);

            if (contractBalance < amount) {
                console.error("WARNING: Contract balance is less than claim amount!");
                console.error("Run the check-balances.ts script to add funds to the contract.");
                return;
            }

            console.log(`\nAttempting to process payout...`);

            // Try to process the payout with higher gas limit
            const tx = await developerPayouts.processPayout(
                taskArgs.issueid,
                {
                    gasLimit: 500000
                }
            );

            console.log(`Transaction sent: ${tx.hash}`);
            console.log("Waiting for confirmation...");

            const receipt = await tx.wait();
            if (receipt === null) {
                console.error("Transaction was mined but receipt is null - check chain for confirmation");
                return;
            }

            console.log(`Transaction confirmed in block ${receipt.blockNumber}`);
            console.log(`Gas used: ${receipt.gasUsed.toString()}`);

            console.log("\nSUCCESS: Processed payout for issue #" + taskArgs.issueid);
        } catch (error: any) {
            console.error("\nError processing payout:", error.message || error);

            // Provide more context for common errors
            if (error.message && error.message.includes("execution reverted")) {
                console.log("\nPossible reasons for reversion:");
                console.log("1. Bounty already paid out");
                console.log("2. Not enough validator approvals");
                console.log("3. Claim expired (more than 7 days old)");
                console.log("4. Contract doesn't have enough funds");
                console.log("\nTry running the scripts directly for more detailed debugging:");
                console.log("npx hardhat run scripts/update-validator.ts --network localhost");
                console.log("npx hardhat run scripts/check-balances.ts --network localhost");
                console.log("npx hardhat run scripts/process-payout.ts --network localhost");
            }
        }
    });

export {};