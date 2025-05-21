import { ethers } from "hardhat";

async function main() {
    const [deployer] = await ethers.getSigners();
    console.log("Deploying FundManager with account:", deployer.address);

    const FundManager = await ethers.getContractFactory("FundManager");
    const fundManager = await FundManager.deploy();
    await fundManager.waitForDeployment();

    const fundManagerAddress = await fundManager.getAddress();
    console.log("FundManager deployed to:", fundManagerAddress);
    return fundManager;
}

if (require.main === module) {
    main()
        .then(() => process.exit(0))
        .catch((error) => {
            console.error(error);
            process.exit(1);
        });
}

export default main;