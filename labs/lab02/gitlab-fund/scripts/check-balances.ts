import { ethers } from "hardhat";

async function main() {
    // Contract addresses - update with your actual addresses
    const FUND_MANAGER_ADDRESS = "0x5FbDB2315678afecb367f032d93F642f64180aa3";
    const DEVELOPER_PAYOUTS_ADDRESS = "0xe7f1725E7734CE288F8367e1Bb143E90bb3F0512";

    console.log("Checking contract balances...");

    // Check FundManager balance
    const fundManagerBalance = await ethers.provider.getBalance(FUND_MANAGER_ADDRESS);
    console.log(`FundManager balance: ${ethers.formatEther(fundManagerBalance)} ETH`);

    // Check DeveloperPayouts balance
    const payoutsBalance = await ethers.provider.getBalance(DEVELOPER_PAYOUTS_ADDRESS);
    console.log(`DeveloperPayouts balance: ${ethers.formatEther(payoutsBalance)} ETH`);

    // Check if DeveloperPayouts has insufficient balance
    if (payoutsBalance === 0n) {
        console.log("DeveloperPayouts has no balance. You might need to transfer ETH to it.");

        // Optional: Automatically transfer some ETH
        const [signer] = await ethers.getSigners();
        console.log(`Using account: ${signer.address} to send funds`);

        const tx = await signer.sendTransaction({
            to: DEVELOPER_PAYOUTS_ADDRESS,
            value: ethers.parseEther("2.0") // Transfer 2 ETH
        });

        await tx.wait();
        console.log(`Transferred 2 ETH to DeveloperPayouts. TX: ${tx.hash}`);

        // Check updated balance
        const newBalance = await ethers.provider.getBalance(DEVELOPER_PAYOUTS_ADDRESS);
        console.log(`New DeveloperPayouts balance: ${ethers.formatEther(newBalance)} ETH`);
    }
}

main()
    .then(() => process.exit(0))
    .catch(error => {
        console.error(error);
        process.exit(1);
    });