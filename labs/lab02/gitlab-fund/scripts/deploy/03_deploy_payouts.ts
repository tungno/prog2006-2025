import { ethers } from "hardhat";
import deployFundManager from "./01_deploy_fund_manager";
import deployValidator from "./02_deploy_validator";

async function main() {
  const [deployer] = await ethers.getSigners();
  console.log("Deploying contracts with account:", deployer.address);

  // Step 1: Deploy FundManager
  console.log("\nStep 1: Deploying FundManager");
  const fundManager = await deployFundManager();
  const fundManagerAddress = await fundManager.getAddress();

  // Step 2: Deploy DeveloperPayouts with a temporary ValidatorMultiSig address
  console.log("\nStep 2: Deploying DeveloperPayouts");
  const DeveloperPayouts = await ethers.getContractFactory("DeveloperPayouts");
  // Deploy with fundManager and a placeholder validator address
  const tempValidatorAddress = "0x0000000000000000000000000000000000000001"; // Using 0x1 as temporary address
  const developerPayouts = await DeveloperPayouts.deploy(
      fundManagerAddress,
      tempValidatorAddress
  );
  await developerPayouts.waitForDeployment();
  const developerPayoutsAddress = await developerPayouts.getAddress();
  console.log("DeveloperPayouts deployed to:", developerPayoutsAddress);

  // Step 3: Deploy ValidatorMultiSig with the actual DeveloperPayouts address
  console.log("\nStep 3: Deploying ValidatorMultiSig");
  const validatorMultiSig = await deployValidator(developerPayoutsAddress);
  const validatorAddress = await validatorMultiSig.getAddress();

  // Step 4: Update DeveloperPayouts with the correct ValidatorMultiSig address
  console.log("\nStep 4: Updating DeveloperPayouts with correct ValidatorMultiSig address");
  // Note: You'll need to add a function to update the validator address in the DeveloperPayouts contract
  // For now, we'll just log that this step needs to be done
  console.log("Note: Add an updateValidator function to DeveloperPayouts contract to update the validator address");

  console.log("\nDeployment Summary:");
  console.log("-------------------");
  console.log("FundManager:", fundManagerAddress);
  console.log("DeveloperPayouts:", developerPayoutsAddress);
  console.log("ValidatorMultiSig:", validatorAddress);
  console.log("\nNext steps:");
  console.log("1. Add updateValidator function to DeveloperPayouts contract");
  console.log("2. Call updateValidator with address:", validatorAddress);

  return {
    fundManager,
    validatorMultiSig,
    developerPayouts
  };
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