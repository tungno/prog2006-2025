import { ethers } from "hardhat";

async function main() {
    try {
        // Contract addresses - use your actual addresses
        const DEVELOPER_PAYOUTS_ADDRESS = "0xe7f1725E7734CE288F8367e1Bb143E90bb3F0512";
        const VALIDATOR_MULTISIG_ADDRESS = "0x9fE46736679d2D9a65F0992F2272dE9f3c7fa6e0";

        console.log(`Updating validator address in DeveloperPayouts...`);
        console.log(`DeveloperPayouts: ${DEVELOPER_PAYOUTS_ADDRESS}`);
        console.log(`ValidatorMultiSig: ${VALIDATOR_MULTISIG_ADDRESS}`);

        // Get signer (this should be the owner of the contract)
        const [owner] = await ethers.getSigners();
        console.log(`Using owner account: ${owner.address}`);

        // Get DeveloperPayouts contract
        const DeveloperPayouts = await ethers.getContractFactory("DeveloperPayouts");
        const developerPayouts = await DeveloperPayouts.attach(DEVELOPER_PAYOUTS_ADDRESS);

        // Update the validator address
        const tx = await developerPayouts.updateValidator(VALIDATOR_MULTISIG_ADDRESS);
        console.log(`Transaction sent: ${tx.hash}`);

        await tx.wait();
        console.log("Validator address updated successfully!");

        // Verify the update
        const newValidatorAddress = await developerPayouts.validatorMultiSig();
        console.log(`New validator address: ${newValidatorAddress}`);

    } catch (error) {
        console.error("Error updating validator address:", error);
    }
}

main()
    .then(() => process.exit(0))
    .catch(error => {
        console.error(error);
        process.exit(1);
    });