import { useState, useEffect } from 'react';
import { EthereumService } from '../services/ethereum/provider';

export function useEthereum() {
    const [service, setService] = useState<EthereumService | null>(null);
    const [account, setAccount] = useState<string>('');
    const [error, setError] = useState<string>('');

    useEffect(() => {
        try {
            const ethService = new EthereumService();
            setService(ethService);
        } catch (err) {
            setError(err instanceof Error ? err.message : 'Failed to initialize Ethereum service');
        }
    }, []);

    const connectWallet = async () => {
        if (!service) return;
        try {
            const address = await service.connectWallet();
            setAccount(address);
            return address;
        } catch (err) {
            setError(err instanceof Error ? err.message : 'Failed to connect wallet');
            throw err;
        }
    };

    return { service, account, error, connectWallet };
}