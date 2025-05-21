import { ethers } from 'ethers';
import { CONTRACT_ADDRESSES, ABIS } from './contracts';

interface TransactionError {
    code: number;
    message: string;
    data?: any;
}

interface BountyDetails {
    amount: string;
    funder: string;
    createdAt: Date;
    active: boolean;
}

interface FunderDetails {
    totalDeposited: string;
    currentBalance: string;
    totalAllocated: string;
    activeBounties: number;
}

interface ClaimStatus {
    claimed: boolean;
    developer: string;
    amount: string;
}

export class EthereumService {
    private provider: ethers.providers.Web3Provider;
    private signer: ethers.Signer;

    public fundManager: ethers.Contract;
    public validatorMultiSig: ethers.Contract;
    public developerPayouts: ethers.Contract;

    constructor() {
        if (!window.ethereum) {
            throw new Error("MetaMask is not installed!");
        }

        this.provider = new ethers.providers.Web3Provider(window.ethereum);
        this.signer = this.provider.getSigner();

        this.fundManager = new ethers.Contract(
            CONTRACT_ADDRESSES.FundManager,
            ABIS.FundManager,
            this.signer
        );

        this.validatorMultiSig = new ethers.Contract(
            CONTRACT_ADDRESSES.ValidatorMultiSig,
            ABIS.ValidatorMultiSig,
            this.signer
        );

        this.developerPayouts = new ethers.Contract(
            CONTRACT_ADDRESSES.DeveloperPayouts,
            ABIS.DeveloperPayouts,
            this.signer
        );
    }

    private async handleTransaction<T>(
        operation: () => Promise<T>,
        errorMessage: string
    ): Promise<T> {
        try {
            return await operation();
        } catch (error) {
            console.error(`${errorMessage}:`, error);
            const txError = error as TransactionError;

            if (txError.code === 4001) {
                throw new Error('Transaction rejected by user');
            }
            if (txError.code === -32603) {
                if (txError.message.includes('insufficient funds')) {
                    throw new Error('Insufficient funds for transaction');
                }
            }

            if (txError.data?.message) {
                throw new Error(txError.data.message);
            }

            throw new Error(errorMessage);
        }
    }

    // Wallet Connection
    async connectWallet(): Promise<string> {
        return await this.handleTransaction(
            async () => {
                await this.provider.send("eth_requestAccounts", []);
                return await this.signer.getAddress();
            },
            'Failed to connect wallet'
        );
    }

    // Funder Methods
    async depositFunds(amount: string): Promise<ethers.ContractTransaction> {
        return await this.handleTransaction(
            async () => {
                const tx = await this.fundManager.depositFunds({
                    value: ethers.utils.parseEther(amount)
                });
                await tx.wait(); // Wait for transaction confirmation
                return tx;
            },
            'Failed to deposit funds'
        );
    }

    async allocateBounty(issueId: number, amount: string): Promise<ethers.ContractTransaction> {
        return await this.handleTransaction(
            async () => {
                const tx = await this.fundManager.allocateBounty(
                    issueId,
                    ethers.utils.parseEther(amount)
                );
                await tx.wait();
                return tx;
            },
            'Failed to allocate bounty'
        );
    }

    async cancelBounty(issueId: number): Promise<ethers.ContractTransaction> {
        return await this.handleTransaction(
            async () => {
                const tx = await this.fundManager.cancelBounty(issueId);
                await tx.wait();
                return tx;
            },
            'Failed to cancel bounty'
        );
    }

    // Developer Methods
    async claimBounty(issueId: number): Promise<ethers.ContractTransaction> {
        return await this.handleTransaction(
            async () => {
                const tx = await this.developerPayouts.claimBounty(issueId);
                await tx.wait();
                return tx;
            },
            'Failed to claim bounty'
        );
    }

    async processPayout(issueId: number): Promise<ethers.ContractTransaction> {
        return await this.handleTransaction(
            async () => {
                const tx = await this.developerPayouts.processPayout(issueId);
                await tx.wait();
                return tx;
            },
            'Failed to process payout'
        );
    }

    // Validator Methods
    async approvePayment(issueId: number, developer: string): Promise<ethers.ContractTransaction> {
        return await this.handleTransaction(
            async () => {
                // First check if the claim exists
                const claimStatus = await this.getClaimStatus(issueId);
                if (!claimStatus.claimed) {
                    throw new Error('No claim exists for this issue');
                }
                if (claimStatus.developer.toLowerCase() !== developer.toLowerCase()) {
                    throw new Error('Developer address does not match claim');
                }

                const tx = await this.validatorMultiSig.approvePayment(issueId, developer);
                await tx.wait();
                return tx;
            },
            'Failed to approve payment'
        );
    }

    async revokeApproval(issueId: number, developer: string): Promise<ethers.ContractTransaction> {
        return await this.handleTransaction(
            async () => {
                const tx = await this.validatorMultiSig.revokeApproval(issueId, developer);
                await tx.wait();
                return tx;
            },
            'Failed to revoke approval'
        );
    }

    // Query Methods
    async getBountyDetails(issueId: number): Promise<BountyDetails> {
        return await this.handleTransaction(
            async () => {
                const details = await this.fundManager.getBountyDetails(issueId);
                return {
                    amount: ethers.utils.formatEther(details.amount),
                    funder: details.funder,
                    createdAt: new Date(details.createdAt.toNumber() * 1000),
                    active: details.active
                };
            },
            'Failed to get bounty details'
        );
    }

    async getFunderDetails(address: string): Promise<FunderDetails> {
        return await this.handleTransaction(
            async () => {
                const details = await this.fundManager.getFunderDetails(address);
                return {
                    totalDeposited: ethers.utils.formatEther(details.totalDeposited),
                    currentBalance: ethers.utils.formatEther(details.currentBalance),
                    totalAllocated: ethers.utils.formatEther(details.totalAllocated),
                    activeBounties: details.activeBounties.toNumber()
                };
            },
            'Failed to get funder details'
        );
    }

    async getClaimStatus(issueId: number): Promise<ClaimStatus> {
        return await this.handleTransaction(
            async () => {
                const [claimed, developer, amount] = await this.developerPayouts.getClaimStatus(issueId);
                return {
                    claimed,
                    developer,
                    amount: ethers.utils.formatEther(amount)
                };
            },
            'Failed to get claim status'
        );
    }

    async isValidator(address: string): Promise<boolean> {
        return await this.handleTransaction(
            async () => {
                const [isActive] = await this.validatorMultiSig.getValidatorStatus(address);
                return isActive;
            },
            'Failed to check validator status'
        );
    }

    async getApprovalCount(issueId: number, developer: string): Promise<number> {
        return await this.handleTransaction(
            async () => {
                const count = await this.validatorMultiSig.getApprovalCount(issueId, developer);
                return count.toNumber();
            },
            'Failed to get approval count'
        );
    }

    // Event Listeners
    async listenToFundingEvents(callback: (event: any) => void) {
        this.fundManager.on('FundsDeposited', callback);
        this.fundManager.on('BountyAllocated', callback);
        this.fundManager.on('BountyCancelled', callback);
    }

    async listenToClaimEvents(callback: (event: any) => void) {
        this.developerPayouts.on('BountyClaimed', callback);
        this.developerPayouts.on('PayoutProcessed', callback);
    }

    async listenToValidatorEvents(callback: (event: any) => void) {
        this.validatorMultiSig.on('PayoutApproved', callback);
        this.validatorMultiSig.on('ApprovalRevoked', callback);
    }

    // Cleanup method
    removeAllListeners() {
        this.fundManager.removeAllListeners();
        this.developerPayouts.removeAllListeners();
        this.validatorMultiSig.removeAllListeners();
    }
}