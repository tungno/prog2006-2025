import React, { useState, useEffect } from 'react';
import { ethers } from "ethers";
import FundManagerABI from '../../../../artifacts/contracts/FundManager.sol/FundManager.json';

export const FunderDashboard: React.FC = () => {
    const [provider, setProvider] = useState<ethers.providers.JsonRpcProvider | null>(null);
    const [fundManagerAddress, setFundManagerAddress] = useState<string>('');
    const [privateKey, setPrivateKey] = useState<string>('');
    const [fundManager, setFundManager] = useState<ethers.Contract | null>(null);
    const [account, setAccount] = useState<string>('');

    const [depositAmount, setDepositAmount] = useState<string>('');
    const [bountyAmount, setBountyAmount] = useState<string>('');
    const [issueId, setIssueId] = useState<string>('');
    const [loading, setLoading] = useState(false);
    const [funderBalance, setFunderBalance] = useState<string>('0');
    const [error, setError] = useState<string | null>(null);
    const [isConnected, setIsConnected] = useState(false);

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
        if (!provider || !privateKey || !fundManagerAddress) {
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
                fundManagerAddress,
                FundManagerABI.abi,
                wallet
            );
            setFundManager(contract);

            // Get balance
            const balance = await contract.getFunderBalance(address);
            setFunderBalance(ethers.utils.formatEther(balance));

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

    // Function to refresh balance
    const refreshBalance = async () => {
        if (!fundManager || !account) return;

        try {
            const balance = await fundManager.getFunderBalance(account);
            setFunderBalance(ethers.utils.formatEther(balance));
        } catch (err) {
            console.error("Error fetching balance:", err);
        }
    };

    // Handle deposit
    const handleDeposit = async () => {
        if (!fundManager || !depositAmount) return;

        setLoading(true);
        try {
            const amount = ethers.utils.parseEther(depositAmount);
            const tx = await fundManager.depositFunds({ value: amount });
            await tx.wait();
            await refreshBalance();

            alert('Deposit successful!');
            setDepositAmount('');
        } catch (err: unknown) {
            console.error('Deposit failed:', err);
            if (err instanceof Error) {
                alert(`Deposit failed: ${err.message}`);
            } else {
                alert("Deposit failed: Unknown error");
            }
        } finally {
            setLoading(false);
        }
    };

    // Handle bounty allocation
    const handleAllocateBounty = async () => {
        if (!fundManager || !bountyAmount || !issueId) return;

        const amount = ethers.utils.parseEther(bountyAmount);
        const balance = ethers.utils.parseEther(funderBalance);

        if (amount.gt(balance)) {
            alert('Insufficient funds. Your balance: ' + funderBalance + ' ETH');
            return;
        }

        setLoading(true);
        try {
            const tx = await fundManager.allocateBounty(Number(issueId), amount);
            await tx.wait();

            alert('Bounty allocated successfully!');
            setBountyAmount('');
            setIssueId('');
            await refreshBalance();
        } catch (err: unknown) {
            console.error('Bounty allocation failed:', err);
            if (err instanceof Error) {
                alert(`Bounty allocation failed: ${err.message}`);
            } else {
                alert("Bounty allocation failed: Unknown error");
            }
        } finally {
            setLoading(false);
        }
    };

    if (error) {
        return <div className="text-red-500 p-4">{error}</div>;
    }

    return (
        <div className="w-full min-h-screen bg-gray-50">
            <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
                <h1 className="text-3xl font-bold text-gray-900 mb-8">Funder Dashboard</h1>

                {!isConnected ? (
                    <div className="bg-white rounded-lg shadow p-6 space-y-4">
                        <h2 className="text-lg font-semibold text-gray-900 mb-4">Connect Wallet</h2>

                        <div>
                            <label className="block text-sm font-medium text-gray-700 mb-1">
                                FundManager Contract Address
                            </label>
                            <input
                                type="text"
                                value={fundManagerAddress}
                                onChange={(e) => setFundManagerAddress(e.target.value)}
                                placeholder="0x5FbDB2315678afecb367f032d93F642f64180aa3"
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
                                placeholder="0xac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80"
                                className="w-full border border-gray-300 rounded-lg px-4 py-2 focus:ring-2 focus:ring-blue-500 focus:border-transparent"
                                disabled={loading}
                            />
                        </div>

                        <button
                            onClick={connectWallet}
                            className="bg-blue-600 hover:bg-blue-700 text-white font-medium px-6 py-3 rounded-lg shadow-sm transition-colors"
                            disabled={loading || !privateKey || !fundManagerAddress}
                        >
                            {loading ? 'Connecting...' : 'Connect Wallet'}
                        </button>
                    </div>
                ) : (
                    <div className="space-y-6">
                        {/* Wallet Info */}
                        <div className="bg-white rounded-lg shadow p-6">
                            <h2 className="text-lg font-semibold text-gray-900 mb-2">Connected Wallet</h2>
                            <p className="text-2xl font-bold">{funderBalance} ETH</p>
                            <div className="flex items-center space-x-2 mb-2">
                                <div className="bg-green-100 rounded-full w-2 h-2"></div>
                                <p className="font-mono text-sm text-gray-600">
                                    {formatAddress(account)}
                                </p>
                            </div>
                            <button
                                onClick={refreshBalance}
                                className="text-blue-600 text-sm hover:text-blue-800"
                            >
                                Refresh Balance
                            </button>
                        </div>

                        {/* Deposit Section */}
                        <div className="bg-white rounded-lg shadow p-6">
                            <h2 className="text-lg font-semibold text-gray-900 mb-4">Deposit Funds</h2>
                            <div className="flex flex-col sm:flex-row gap-4">
                                <input
                                    type="number"
                                    value={depositAmount}
                                    onChange={(e) => setDepositAmount(e.target.value)}
                                    placeholder="Amount in ETH"
                                    className="flex-1 border border-gray-300 rounded-lg px-4 py-2 focus:ring-2 focus:ring-blue-500 focus:border-transparent"
                                    disabled={loading}
                                />
                                <button
                                    onClick={handleDeposit}
                                    className="bg-green-500 hover:bg-green-600 text-white font-medium px-6 py-2 rounded-lg disabled:opacity-50 transition-colors w-full sm:w-auto"
                                    disabled={loading || !depositAmount}
                                >
                                    {loading ? 'Processing...' : 'Deposit'}
                                </button>
                            </div>
                        </div>

                        {/* Bounty Allocation Section */}
                        <div className="bg-white rounded-lg shadow p-6">
                            <h2 className="text-lg font-semibold text-gray-900 mb-4">Allocate Bounty</h2>
                            <div className="space-y-4">
                                <div>
                                    <label className="block text-sm font-medium text-gray-700 mb-1">
                                        GitLab Issue ID
                                    </label>
                                    <input
                                        type="number"
                                        value={issueId}
                                        onChange={(e) => setIssueId(e.target.value)}
                                        placeholder="Enter issue ID"
                                        className="w-full border border-gray-300 rounded-lg px-4 py-2 focus:ring-2 focus:ring-blue-500 focus:border-transparent"
                                        disabled={loading}
                                    />
                                </div>
                                <div>
                                    <label className="block text-sm font-medium text-gray-700 mb-1">
                                        Bounty Amount
                                    </label>
                                    <div className="flex flex-col sm:flex-row gap-4">
                                        <input
                                            type="number"
                                            value={bountyAmount}
                                            onChange={(e) => setBountyAmount(e.target.value)}
                                            placeholder="Amount in ETH"
                                            className="flex-1 border border-gray-300 rounded-lg px-4 py-2 focus:ring-2 focus:ring-blue-500 focus:border-transparent"
                                            disabled={loading}
                                        />
                                        <button
                                            onClick={handleAllocateBounty}
                                            className="bg-blue-600 hover:bg-blue-700 text-white font-medium px-6 py-2 rounded-lg disabled:opacity-50 transition-colors w-full sm:w-auto"
                                            disabled={loading || !bountyAmount || !issueId}
                                        >
                                            {loading ? 'Processing...' : 'Allocate Bounty'}
                                        </button>
                                    </div>
                                </div>
                            </div>
                        </div>
                    </div>
                )}
            </div>
        </div>
    );
};