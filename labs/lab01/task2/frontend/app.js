// Hardcoded contract address after deployment on Sepolia
const contractAddress = "0x4a518931404F1305560cc5Ee4F60d8cbdaAA8Dc9";

let contract;
let signer;

async function init() {
  try {
    if (!window.ethereum) {
      alert("MetaMask not detected!");
      return;
    }

    const provider = new ethers.providers.Web3Provider(window.ethereum);
    await window.ethereum.request({ method: "eth_requestAccounts" });
    signer = provider.getSigner();

    // Display connected account
    document.getElementById("walletstatus").innerText = await signer.getAddress();

    // Fetch the contract ABI
    const res = await fetch("./RockPaperScissors.json");
    const artifact = await res.json();

    // Initialize contract
    contract = new ethers.Contract(contractAddress, artifact.abi, signer);

    console.log("Contract initialized at:", contract.address);
  } catch (error) {
    console.error("Error initializing contract:", error);
  }
}

document.getElementById("connectWallet").addEventListener("click", init);

// PLAY (commit hashed vote)
document.getElementById("playButton").addEventListener("click", async () => {
  try {
    if (!contract) throw new Error("Contract not initialized");

    const hashedMove = document.getElementById("hashedMove").value;
    const stake = document.getElementById("playStake").value;

    if (!hashedMove || !stake) throw new Error("Please enter hashed move and stake amount.");

    const tx = await contract.play(hashedMove, {
      value: ethers.utils.parseEther(stake),
    });

    document.getElementById("playStatus").innerText = `Transaction sent: ${tx.hash}`;
    await tx.wait();
    document.getElementById("playStatus").innerText = "Play/commit successful!";
  } catch (err) {
    document.getElementById("playStatus").innerText = "Error: " + err.message;
  }
});

// REVEAL
document.getElementById("revealButton").addEventListener("click", async () => {
  try {
    if (!contract) throw new Error("Contract not initialized");

    const move = document.getElementById("revealMove").value;
    const salt = document.getElementById("revealSalt").value;

    if (!move || !salt) throw new Error("Move or salt missing.");

    const tx = await contract.reveal(move, salt);

    document.getElementById("revealStatus").innerText = `Transaction sent: ${tx.hash}`;
    await tx.wait();
    document.getElementById("revealStatus").innerText = "Reveal successful!";
  } catch (err) {
    document.getElementById("revealStatus").innerText = "Error: " + err.message;
  }
});

// WITHDRAW
document.getElementById("withdrawButton").addEventListener("click", async () => {
  try {
    if (!contract) throw new Error("Contract not initialized");

    // Check if the user has funds to withdraw before attempting the transaction
    const userAddress = await signer.getAddress();
    const pendingAmount = await contract.pendingWithdrawals(userAddress);

    if (pendingAmount.toString() === "0") {
      document.getElementById("withdrawStatus").innerText = 
        "❌ Sorry, you lost this time! Hope you win next time. Try again! 🎮";
      return;
    }

    const tx = await contract.withdraw();
    document.getElementById("withdrawStatus").innerText = `Transaction sent: ${tx.hash}`;
    await tx.wait();

    // Display success message for winners
    document.getElementById("withdrawStatus").innerText = 
      `🎉 Congratulations! You win! 🏆 Withdraw executed successfully! ✅`;

  } catch (err) {
    document.getElementById("withdrawStatus").innerText = "Error: " + err.message;
  }
});



// Generate Hash
document.getElementById("generateHashBtn").addEventListener("click", () => {
  const move = document.getElementById("genMove").value.trim();
  const salt = document.getElementById("genSalt").value.trim();

  if (!move || !salt) {
    alert("Please enter both move and salt");
    return;
  }

  // Check that move is one of R, P, S
  if (!["R", "P", "S"].includes(move.toUpperCase())) {
    alert("Move must be R, P, or S");
    return;
  }

  // Generate hashed move using Ethers
  const hashed = ethers.utils.solidityKeccak256(["string", "string"], [move, salt]);

  // Display it in the HTML
  document.getElementById("genHashOutput").innerText = hashed;
  // Also populate the hashedMove input so the user can just click "Play"
  document.getElementById("hashedMove").value = hashed;
});
