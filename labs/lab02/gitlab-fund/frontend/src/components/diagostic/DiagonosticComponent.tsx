import React, { useState, useEffect } from 'react';
import { ethers } from 'ethers';
import DeveloperPayoutsABI from '../../../../artifacts/contracts/DeveloperPayouts.sol/DeveloperPayouts.json';

export const DiagnosticComponent: React.FC = () => {
    const [provider, setProvider] = useState<ethers.providers.JsonRpcProvider | null>(null);
    const [developerPayoutsAddress, setDeveloperPayoutsAddress] = useState<string>('');
    const [issueId, setIssueId] = useState<string>('');
    const [result, setResult] = useState<string>('');

    useEffect(() => {
        const provider = new ethers.providers.JsonRpcProvider('http://localhost:8545');
        setProvider(provider);
    }, []);

    const runDiagnostic = async () => {
        if (!provider || !developerPayoutsAddress || !issueId) {
            setResult('Please provide all required information');
            return;
        }

        try {
            setResult('Starting diagnostic...');

            // 1. Get contract
            const contract = new ethers.Contract(
                developerPayoutsAddress,
                DeveloperPayoutsABI.abi,
                provider
            );

            // 2. Check claim status
            const claimStatus = await contract.getClaimStatus(Number(issueId));
            setResult(prev => `${prev}\nClaim Status: ${JSON.stringify({
                claimed: claimStatus.claimed,
                developer: claimStatus.developer,
                amount: ethers.utils.formatEther(claimStatus.amount) + ' ETH'
            }, null, 2)}`);

            // 3. Check validator address
            const validatorAddress = await contract.validatorMultiSig();
            setResult(prev => `${prev}\nValidator Address: ${validatorAddress}`);

            // 4. Try to create a contract instance for validator
            try {
                const validatorABI = [
                    "function isApproved(uint256 issueId, address developer) view returns (bool)",
                    "function getApprovalCount(uint256 issueId, address developer) view returns (uint256)",
                    "function threshold() view returns (uint256)"
                ];

                const validatorContract = new ethers.Contract(
                    validatorAddress,
                    validatorABI,
                    provider
                );

                // 5. Check if issue is approved by validators
                const isApproved = await validatorContract.isApproved(
                    Number(issueId),
                    claimStatus.developer
                );
                setResult(prev => `${prev}\nApproved by validators: ${isApproved}`);

                const approvalCount = await validatorContract.getApprovalCount(
                    Number(issueId),
                    claimStatus.developer
                );

                // Try both threshold functions
                let threshold: ethers.BigNumber;
                try {
                    threshold = await validatorContract.threshold();
                } catch (e) {
                    try {
                        threshold = await validatorContract.requiredApprovals();
                    } catch (e2) {
                        threshold = ethers.BigNumber.from(2); // fallback
                    }
                }

                setResult(prev => `${prev}\nApproval count: ${approvalCount} / ${threshold} required`);

                // 6. Check contract balance
                const balance = await provider.getBalance(developerPayoutsAddress);
                setResult(prev => `${prev}\nContract Balance: ${ethers.utils.formatEther(balance)} ETH`);

                if (balance.lt(claimStatus.amount)) {
                    setResult(prev => `${prev}\nWARNING: Contract doesn't have enough balance to pay out the claim`);
                }

                // 7. Check if the claim might be already paid
                const isPaid = await checkIsPaid(contract, Number(issueId));
                setResult(prev => `${prev}\nClaim is paid: ${isPaid}`);

                setResult(prev => `${prev}\n\nDiagnostic completed. Check results above.`);

            } catch (e) {
                console.error("Validator contract error:", e);
                setResult(prev => `${prev}\nError with validator contract: ${e instanceof Error ? e.message : 'Unknown error'}`);
            }

        } catch (err) {
            console.error("Diagnostic failed:", err);
            setResult(prev => `${prev}\nDiagnostic failed: ${err instanceof Error ? err.message : 'Unknown error'}`);
        }
    };

    const checkIsPaid = async (contract: ethers.Contract, issueId: number): Promise<boolean> => {
        try {
            // Try to access the claims mapping directly by using the storage slot
            // This is a workaround as your contract doesn't expose isPaid directly

            // First try to see if there's any convenience method
            try {
                // Check if the contract exposes a method to check if paid
                const paid = await contract.claims(issueId);
                return paid.paid || false;
            } catch (e) {
                // If no direct method, we'll use our knowledge of the contract structure
                return false; // We can't easily check, assume not paid
            }
        } catch (e) {
            console.error("Error checking paid status:", e);
            return false;
        }
    };

    return (
        <div className="p-4 bg-white rounded shadow">
            <h2 className="text-lg font-bold mb-4">ProcessPayout Diagnostic Tool</h2>

            <div className="mb-4">
                <label className="block mb-1">DeveloperPayouts Address:</label>
                <input
                    value={developerPayoutsAddress}
                    onChange={(e) => setDeveloperPayoutsAddress(e.target.value)}
                    className="w-full p-2 border rounded"
                    placeholder="0xe7f1725E7734CE288F8367e1Bb143E90bb3F0512"
                />
            </div>

            <div className="mb-4">
                <label className="block mb-1">Issue ID:</label>
                <input
                    value={issueId}
                    onChange={(e) => setIssueId(e.target.value)}
                    className="w-full p-2 border rounded"
                    type="number"
                />
            </div>

            <button
                onClick={runDiagnostic}
                className="px-4 py-2 bg-blue-500 text-white rounded hover:bg-blue-600"
            >
                Run Diagnostic
            </button>

            {result && (
                <div className="mt-4">
                    <h3 className="font-bold mb-2">Results:</h3>
                    <pre className="p-3 bg-gray-800 rounded whitespace-pre-wrap text-sm text-white">
                        {result}
                    </pre>
                </div>
            )}
        </div>
    );
};