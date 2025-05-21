import React, { useState, useEffect } from 'react';
import { ethers } from 'ethers';
import DeveloperPayoutsABI from '../../../../artifacts/contracts/DeveloperPayouts.sol/DeveloperPayouts.json';

interface ClaimStatus {
    claimed: boolean;
    developer: string;
    amount: string;
}

export const DeveloperDashboard: React.FC = () => {
    const [provider, setProvider] = useState<ethers.providers.JsonRpcProvider | null>(null);
    const [developerPayoutsAddress, setDeveloperPayoutsAddress] = useState<string>('');
    const [privateKey, setPrivateKey] = useState<string>('');
    const [developerPayouts, setDeveloperPayouts] = useState<ethers.Contract | null>(null);
    const [account, setAccount] = useState<string>('');

    const [issueId, setIssueId] = useState<string>('');
    const [loading, setLoading] = useState(false);
    const [claimStatus, setClaimStatus] = useState<ClaimStatus | null>(null);
    const [processingPayout, setProcessingPayout] = useState(false);
    const [error, setError] = useState<string | null>(null);
    const [isConnected, setIsConnected] = useState(false);
    const [debugInfo, setDebugInfo] = useState<string>('');

    // Truncate wallet address for display
    const formatAddress = (address: string) => {
        return `${address.substring(0, 6)}...${address.substring(address.length - 4)}`;
    };

    // Initialize provider
    useEffect(() => {
        try {
            const newProvider = new ethers.providers.JsonRpcProvider('http://localhost:8545');
            setProvider(newProvider);
        } catch (err) {
            console.error("Failed to connect to provider:", err);
            setError("Failed to connect to the local blockchain. Make sure your node is running.");
        }
    }, []);

    // Connect wallet with provided private key and contract address
    const connectWallet = async () => {
        if (!provider || !privateKey || !developerPayoutsAddress) {
            setError("Please provide both private key and contract address");
            return;
        }

        try {
            setLoading(true);

            // Create wallet
            const wallet = new ethers.Wallet(privateKey, provider);
            const address = await wallet.getAddress();
            setAccount(address);

            // Initialize contract
            const contract = new ethers.Contract(
                developerPayoutsAddress,
                DeveloperPayoutsABI.abi,
                wallet
            );
            setDeveloperPayouts(contract);

            setIsConnected(true);
            setError(null);
        } catch (err: unknown) {
            console.error("Connection error:", err);
            if (err instanceof Error) {
                setError(err.message);
            } else {
                setError("Failed to connect. Check your private key and contract address.");
            }
        } finally {
            setLoading(false);
        }
    };

    // Handle claim bounty
    const handleClaimBounty = async () => {
        if (!developerPayouts || !issueId) return;

        setLoading(true);
        try {
            const tx = await developerPayouts.claimBounty(Number(issueId));
            setDebugInfo(`Claim TX Hash: ${tx.hash}`);
            await tx.wait();

            alert('Bounty claimed successfully!');
            await checkClaimStatus();
        } catch (err: unknown) {
            console.error('Failed to claim bounty:', err);
            if (err instanceof Error) {
                alert(`Failed to claim bounty: ${err.message}`);
            } else {
                alert('Failed to claim bounty: Unknown error');
            }
        } finally {
            setLoading(false);
        }
    };

    // Check claim status
    const checkClaimStatus = async () => {
        if (!developerPayouts || !issueId) return;

        try {
            const status = await developerPayouts.getClaimStatus(Number(issueId));
            setClaimStatus({
                claimed: status.claimed,
                developer: status.developer,
                amount: ethers.utils.formatEther(status.amount)
            });

            // Also check if payout is already processed
            try {
                const isPaid = await developerPayouts.isPaid(Number(issueId));
                setDebugInfo(prev => `${prev}\nIssue already paid: ${isPaid}`);
            } catch (e) {
                // Function might not exist, just log for debugging
                console.log("Could not check if issue is already paid:", e);
            }

        } catch (err: unknown) {
            console.error('Error checking claim status:', err);
            if (err instanceof Error) {
                alert(`Error checking claim status: ${err.message}`);
            }
        }
    };

    // Process payout - first approach: manual transaction
    const handleProcessPayout = async () => {
        if (!developerPayouts || !issueId || !provider) return;

        setProcessingPayout(true);
        try {
            // Create a wallet to sign with
            const wallet = new ethers.Wallet(privateKey, provider);

            // Get the contract interface
            const iface = new ethers.utils.Interface(DeveloperPayoutsABI.abi);

            // Encode the function call
            const data = iface.encodeFunctionData("processPayout", [Number(issueId)]);

            // Create the transaction
            const tx = {
                to: developerPayoutsAddress,
                data: data,
                gasLimit: ethers.utils.hexlify(500000), // Higher gas limit
            };

            // Send the transaction
            const response = await wallet.sendTransaction(tx);
            setDebugInfo(prev => `${prev}\nManual TX Hash: ${response.hash}`);

            // Wait for it to be mined
            const receipt = await response.wait();

            // Check status
            if (receipt.status === 1) {
                alert('Payout processed successfully!');
            } else {
                alert('Transaction failed! See console for details.');
                console.error("Transaction failed", receipt);
            }

            // Refresh claim status
            await checkClaimStatus();

        } catch (err: unknown) {
            console.error('Failed to process payout:', err);

            // Add detailed error info to debug
            if (err instanceof Error) {
                const errorMessage = err.message;
                setDebugInfo(prev => `${prev}\nError: ${errorMessage}`);

                // More user-friendly messages
                if (errorMessage.includes("execution reverted")) {
                    alert("Failed to process payout: Transaction reverted. This might be because the payout was already processed or there aren't enough approvals.");
                } else {
                    alert(`Failed to process payout: ${errorMessage}`);
                }
            } else {
                alert('Failed to process payout: Unknown error');
            }
        } finally {
            setProcessingPayout(false);
        }
    };

    // Alternative process payout - direct call with override
    const handleProcessPayoutAlternative = async () => {
        if (!developerPayouts || !issueId) return;

        setProcessingPayout(true);
        try {
            // Try manually constructing call with overrides
            const gasLimit = ethers.utils.hexlify(500000);
            const overrides = {
                gasLimit: gasLimit,
                gasPrice: await provider?.getGasPrice()
            };

            // Process payout with overrides
            const tx = await developerPayouts.processPayout(Number(issueId), overrides);
            setDebugInfo(prev => `${prev}\nAlternative TX Hash: ${tx.hash}`);

            await tx.wait();
            alert('Payout processed successfully!');
            await checkClaimStatus();

        } catch (err: unknown) {
            console.error('Failed to process payout (alternative):', err);

            if (err instanceof Error) {
                const errorMessage = err.message;
                setDebugInfo(prev => `${prev}\nAlt Error: ${errorMessage}`);

                // Show more technical details for debugging
                alert(`Failed to process payout: ${errorMessage.substring(0, 200)}...`);
            } else {
                alert('Failed to process payout: Unknown error');
            }
        } finally {
            setProcessingPayout(false);
        }
    };

    if (error) {
        return <div className="text-red-500 p-4">{error}</div>;
    }

    return (
        <div className="w-full min-h-screen bg-gray-50">
            <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
                <h1 className="text-3xl font-bold text-gray-900 mb-8">Developer Dashboard</h1>

                {!isConnected ? (
                    <div className="bg-white rounded-lg shadow p-6 space-y-4">
                        <h2 className="text-lg font-semibold text-gray-900 mb-4">Connect Wallet</h2>

                        <div>
                            <label className="block text-sm font-medium text-gray-700 mb-1">
                                DeveloperPayouts Contract Address
                            </label>
                            <input
                                type="text"
                                value={developerPayoutsAddress}
                                onChange={(e) => setDeveloperPayoutsAddress(e.target.value)}
                                placeholder="0xe7f1725E7734CE288F8367e1Bb143E90bb3F0512"
                                className="w-full border border-gray-300 rounded-lg px-4 py-2 focus:ring-2 focus:ring-blue-500 focus:border-transparent"
                                disabled={loading}
                            />
                        </div>

                        <div>
                            <label className="block text-sm font-medium text-gray-700 mb-1">
                                Private Key
                            </label>
                            <input
                                type="password"
                                value={privateKey}
                                onChange={(e) => setPrivateKey(e.target.value)}
                                placeholder="0x47e179ec197488593b187f80a00eb0da91f1b9d0b13f8733639f19c30a34926a"
                                className="w-full border border-gray-300 rounded-lg px-4 py-2 focus:ring-2 focus:ring-blue-500 focus:border-transparent"
                                disabled={loading}
                            />
                        </div>

                        <button
                            onClick={connectWallet}
                            className="bg-blue-600 hover:bg-blue-700 text-white font-medium px-6 py-3 rounded-lg shadow-sm transition-colors"
                            disabled={loading || !privateKey || !developerPayoutsAddress}
                        >
                            {loading ? 'Connecting...' : 'Connect Wallet'}
                        </button>
                    </div>
                ) : (
                    <div className="space-y-6">
                        {/* Wallet Info */}
                        <div className="bg-white rounded-lg shadow p-6">
                            <h2 className="text-lg font-semibold text-gray-900 mb-2">Connected Wallet</h2>
                            <div className="flex items-center space-x-2">
                                <div className="bg-green-100 rounded-full w-2 h-2"></div>
                                <p className="font-mono text-sm text-gray-600">
                                    {formatAddress(account)}
                                </p>
                            </div>
                        </div>

                        {/* Claim Bounty Section */}
                        <div className="bg-white rounded-lg shadow p-6">
                            <h2 className="text-lg font-semibold text-gray-900 mb-4">Claim Bounty</h2>
                            <div className="space-y-4">
                                <div>
                                    <label className="block text-sm font-medium text-gray-700 mb-1">
                                        GitLab Issue ID
                                    </label>
                                    <div className="flex flex-col sm:flex-row gap-4">
                                        <input
                                            type="number"
                                            value={issueId}
                                            onChange={(e) => setIssueId(e.target.value)}
                                            placeholder="Enter issue ID"
                                            className="flex-1 border border-gray-300 rounded-lg px-4 py-2 focus:ring-2 focus:ring-blue-500 focus:border-transparent"
                                            disabled={loading}
                                        />
                                        <button
                                            onClick={handleClaimBounty}
                                            className="bg-blue-600 hover:bg-blue-700 text-white font-medium px-6 py-2 rounded-lg disabled:opacity-50 transition-colors w-full sm:w-auto"
                                            disabled={loading || !issueId}
                                        >
                                            {loading ? 'Processing...' : 'Claim Bounty'}
                                        </button>
                                    </div>
                                </div>
                                <button
                                    onClick={checkClaimStatus}
                                    className="text-blue-600 text-sm hover:text-blue-800"
                                    disabled={!issueId}
                                >
                                    Check Claim Status
                                </button>
                            </div>
                        </div>

                        {/* Claim Status Display */}
                        {claimStatus && (
                            <div className="bg-white rounded-lg shadow p-6">
                                <h2 className="text-lg font-semibold text-gray-900 mb-4">Claim Status</h2>
                                <div className="space-y-2">
                                    <p>Status: {claimStatus.claimed ? 'Claimed' : 'Not Claimed'}</p>
                                    {claimStatus.claimed && (
                                        <>
                                            <p>Claimed by: {formatAddress(claimStatus.developer)}</p>
                                            <p>Amount: {claimStatus.amount} ETH</p>
                                            <div className="mt-4 space-y-2">
                                                <div className="flex flex-col sm:flex-row gap-4">
                                                    <button
                                                        onClick={handleProcessPayout}
                                                        className="bg-green-600 hover:bg-green-700 text-white font-medium px-6 py-2 rounded-lg disabled:opacity-50 transition-colors"
                                                        disabled={processingPayout}
                                                    >
                                                        {processingPayout ? 'Processing...' : 'Process Payout (Manual)'}
                                                    </button>
                                                    <button
                                                        onClick={handleProcessPayoutAlternative}
                                                        className="bg-blue-600 hover:bg-blue-700 text-white font-medium px-6 py-2 rounded-lg disabled:opacity-50 transition-colors"
                                                        disabled={processingPayout}
                                                    >
                                                        {processingPayout ? 'Processing...' : 'Process Payout (Alt)'}
                                                    </button>
                                                </div>
                                                <small className="text-gray-500 block mt-2">
                                                    If one method fails, try the other. The first attempts a manual transaction construction,
                                                    the second uses contract call with explicit gas parameters.
                                                </small>
                                            </div>
                                        </>
                                    )}
                                </div>
                            </div>
                        )}

                        {/* Debug Info Display */}
                        {debugInfo && (
                            <div className="bg-gray-100 rounded-lg shadow p-6">
                                <h2 className="text-lg font-semibold text-gray-900 mb-4">Debug Information</h2>
                                <pre className="whitespace-pre-wrap text-xs font-mono bg-gray-800 text-white p-4 rounded overflow-auto">
                                    {debugInfo}
                                </pre>
                            </div>
                        )}
                    </div>
                )}
            </div>
        </div>
    );
};