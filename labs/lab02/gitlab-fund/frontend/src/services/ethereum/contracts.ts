import FundManagerABI from '../../../../artifacts/contracts/FundManager.sol/FundManager.json'
import ValidatorMultiSigABI from '../../../../artifacts/contracts/ValidatorMultiSig.sol/ValidatorMultiSig.json';
import DeveloperPayoutsABI from '../../../../artifacts/contracts/DeveloperPayouts.sol/DeveloperPayouts.json'

export const CONTRACT_ADDRESSES = {
    FundManager: "0x5FbDB2315678afecb367f032d93F642f64180aa3",
    ValidatorMultiSig: "0xe7f1725E7734CE288F8367e1Bb143E90bb3F0512",
    DeveloperPayouts: "0x9fE46736679d2D9a65F0992F2272dE9f3c7fa6e0"
};

export const ABIS = {
    FundManager: FundManagerABI.abi,
    ValidatorMultiSig: ValidatorMultiSigABI.abi,
    DeveloperPayouts: DeveloperPayoutsABI.abi
};