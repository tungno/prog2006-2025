import { ethers } from "hardhat";

async function main(developerPayoutsAddress: string) {
  const [deployer, validator1, validator2, validator3] = await ethers.getSigners();

  const initialValidators = [
    validator1.address,
    validator2.address,
    validator3.address
  ];
  const threshold = 2; // 2 out of 3 validators required

  console.log("Deploying ValidatorMultiSig with account:", deployer.address);
  console.log("Initial validators:", initialValidators);
  console.log("Threshold:", threshold);
  console.log("DeveloperPayouts address:", developerPayoutsAddress);

  const ValidatorMultiSig = await ethers.getContractFactory("ValidatorMultiSig");
  const validatorMultiSig = await ValidatorMultiSig.deploy(
      initialValidators,
      threshold,
      developerPayoutsAddress
  );
  await validatorMultiSig.waitForDeployment();

  const validatorAddress = await validatorMultiSig.getAddress();
  console.log("ValidatorMultiSig deployed to:", validatorAddress);
  return validatorMultiSig;
}

if (require.main === module) {
  // This won't be called directly anymore
  console.log("This script should be called from 03_deploy_payouts.ts");
  process.exit(1);
}

export default main;