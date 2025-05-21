// scripts/deploy.js
async function main() {
    const [deployer] = await ethers.getSigners();
  
    console.log("Deploying contract with account:", deployer.address);
  
    const RPSFactory = await ethers.getContractFactory("RockPaperScissors");
    const rps = await RPSFactory.deploy();
    await rps.deployed();
  
    console.log("RockPaperScissors deployed to:", rps.address);
  }
  
  main().catch((error) => {
    console.error(error);
    process.exitCode = 1;
  });
  