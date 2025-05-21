import React, { useState, useEffect } from 'react';
import { ethers } from 'ethers';
import ValidatorMultiSigABI from '../../../../artifacts/contracts/ValidatorMultiSig.sol/ValidatorMultiSig.json';

interface ApprovalStatus {
    approved: boolean;
    approvalCount: number;
    requiredApprovals: number;
}

export const ValidatorDashboard: React.FC = () => {
    const [provider, setProvider] = useState<ethers.providers.JsonRpcProvider | null>(null);
    const [validatorAddress, setValidatorAddress] = useState<string>('');
    const [privateKey, setPrivateKey] = useState<string>('');
    const [validatorMultiSig, setValidatorMultiSig] = useState<ethers.Contract | null>(null);
    const [account, setAccount] = useState<string>('');

    const [issueId, setIssueId] = useState<string>('');
    const [developerAddress, setDeveloperAddress] = useState<string>('');
    const [loading, setLoading] = useState(false);
    const [approvalStatus, setApprovalStatus] = useState<ApprovalStatus | null>(null);
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
        if (!provider || !privateKey || !validatorAddress) {
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
                validatorAddress,
                ValidatorMultiSigABI.abi,
                wallet
            );
            setValidatorMultiSig(contract);

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

    // Handle approve payment
    const handleApprovePayment = async () => {
        if (!validatorMultiSig || !issueId || !developerAddress) return;

        setLoading(true);
        try {
            const tx = await validatorMultiSig.approvePayment(Number(issueId), developerAddress);
            await tx.wait();

            alert('Payment approved successfully!');
            await checkApprovalStatus();
        } catch (err: unknown) {
            console.error('Failed to approve payment:', err);
            if (err instanceof Error) {
                alert(`Failed to approve payment: ${err.message}`);
            } else {
                alert("Failed to approve payment: Unknown error");
            }
        } finally {
            setLoading(false);
        }
    };

    // Check approval status
    const checkApprovalStatus = async () => {
        if (!validatorMultiSig || !issueId || !developerAddress) return;

        try {
            const approved = await validatorMultiSig.isApproved(Number(issueId), developerAddress);
            const approvalCount = await validatorMultiSig.getApprovalCount(Number(issueId), developerAddress);

            // Get threshold from contract - handle both function name possibilities
            let requiredApprovals;
            try {
                // Try threshold() first
                requiredApprovals = await validatorMultiSig.threshold();
            } catch (e) {
                try {
                    // Fall back to requiredApprovals() if threshold() doesn't exist
                    requiredApprovals = await validatorMultiSig.requiredApprovals();
                } catch (e2) {
                    console.error("Could not find threshold or requiredApprovals function");
                    requiredApprovals = ethers.BigNumber.from(2); // Default fallback
                }
            }

            setApprovalStatus({
                approved,
                approvalCount: approvalCount.toNumber(),
                requiredApprovals: requiredApprovals.toNumber()
            });
        } catch (err: unknown) {
            console.error('Error checking approval status:', err);
            if (err instanceof Error) {
                alert(`Error checking status: ${err.message}`);
            }
        }
    };

    if (error) {
        return <div className="text-red-500 p-4">{error}</div>;
    }

    return (
        <div className="w-full min-h-screen bg-gray-50">
            <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
                <h1 className="text-3xl font-bold text-gray-900 mb-8">Validator Dashboard</h1>

                {!isConnected ? (
                    <div className="bg-white rounded-lg shadow p-6 space-y-4">
                        <h2 className="text-lg font-semibold text-gray-900 mb-4">Connect Wallet</h2>

                        <div>
                            <label className="block text-sm font-medium text-gray-700 mb-1">
                                ValidatorMultiSig Contract Address
                            </label>
                            <input
                                type="text"
                                value={validatorAddress}
                                onChange={(e) => setValidatorAddress(e.target.value)}
                                placeholder="0x9fE46736679d2D9a65F0992F2272dE9f3c7fa6e0"
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
                                placeholder="0x59c6995e998f97a5a0044966f0945389dc9e86dae88c7a8412f4603b6b78690d"
                                className="w-full border border-gray-300 rounded-lg px-4 py-2 focus:ring-2 focus:ring-blue-500 focus:border-transparent"
                                disabled={loading}
                            />
                        </div>

                        <button
                            onClick={connectWallet}
                            className="bg-blue-600 hover:bg-blue-700 text-white font-medium px-6 py-3 rounded-lg shadow-sm transition-colors"
                            disabled={loading || !privateKey || !validatorAddress}
                        >
                            {loading ? 'Connecting...' : 'Connect Wallet'}
                        </button>
                    </div>
                ) : (
                    <div className="space-y-6">
                        {/* Wallet Info */}
                        <div className="bg-white rounded-lg shadow p-6">
                            <h2 className="text-lg font-semibold text-gray-900 mb-2">Connected Validator</h2>
                            <div className="flex items-center space-x-2">
                                <div className="bg-green-100 rounded-full w-2 h-2"></div>
                                <p className="font-mono text-sm text-gray-600">
                                    {formatAddress(account)}
                                </p>
                            </div>
                        </div>

                        {/* Approval Section */}
                        <div className="bg-white rounded-lg shadow p-6">
                            <h2 className="text-lg font-semibold text-gray-900 mb-4">Approve Payment</h2>
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
                                        Developer Address
                                    </label>
                                    <input
                                        type="text"
                                        value={developerAddress}
                                        onChange={(e) => setDeveloperAddress(e.target.value)}
                                        placeholder="0x15d34AAf54267DB7D7c367839AAf71A00a2C6A65"
                                        className="w-full border border-gray-300 rounded-lg px-4 py-2 focus:ring-2 focus:ring-blue-500 focus:border-transparent"
                                        disabled={loading}
                                    />
                                </div>
                                <div className="flex flex-col sm:flex-row gap-4">
                                    <button
                                        onClick={handleApprovePayment}
                                        className="bg-green-600 hover:bg-green-700 text-white font-medium px-6 py-2 rounded-lg disabled:opacity-50 transition-colors w-full sm:w-auto"
                                        disabled={loading || !issueId || !developerAddress}
                                    >
                                        {loading ? 'Processing...' : 'Approve Payment'}
                                    </button>
                                    <button
                                        onClick={checkApprovalStatus}
                                        className="bg-blue-600 hover:bg-blue-700 text-white font-medium px-6 py-2 rounded-lg disabled:opacity-50 transition-colors w-full sm:w-auto"
                                        disabled={loading || !issueId || !developerAddress}
                                    >
                                        Check Status
                                    </button>
                                </div>
                            </div>
                        </div>

                        {/* Approval Status */}
                        {approvalStatus && (
                            <div className="bg-white rounded-lg shadow p-6">
                                <h2 className="text-lg font-semibold text-gray-900 mb-4">Approval Status</h2>
                                <div className="space-y-2">
                                    <p>Status: {approvalStatus.approved ? 'Fully Approved' : 'Pending'}</p>
                                    <p>Approval
                                        Count: {approvalStatus.approvalCount} / {approvalStatus.requiredApprovals} required</p>
                                    <div className="w-full bg-gray-200 rounded-full h-2.5 mt-2">
                                        <div
                                            className="bg-blue-600 h-2.5 rounded-full"
                                            style={{width: `${(approvalStatus.approvalCount / approvalStatus.requiredApprovals) * 100}%`}}
                                        ></div>
                                    </div>
                                </div>
                            </div>
                        )}
                    </div>
                )}
            </div>
        </div>
    );
};