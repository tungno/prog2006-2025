// SPDX-License-Identifier: MIT
pragma solidity ^0.8.19;

/**
 * @title DeveloperPayouts
 * @dev Contract for managing developer bounty claims and payouts
 * This contract allows developers to claim bounties for resolved issues
 * and processes payouts once validators approve the claims.
 */

import "./interfaces/IDeveloperPayouts.sol";
import "./interfaces/IFundManager.sol";
import "./interfaces/IValidatorMultiSig.sol";
import "./libraries/SecurityUtils.sol";

contract DeveloperPayouts is IDeveloperPayouts {
    using SecurityUtils for SecurityUtils.NonReentrantContext;

    // Non-reentrant context to prevent reentrancy attacks
    SecurityUtils.NonReentrantContext private _reentrancyContext;

    /**
     * @dev Struct to store all information related to a bounty claim
     * @param developer Address of the developer who claimed the bounty
     * @param amount The amount of the bounty in wei
     * @param claimed Boolean indicating if the bounty has been claimed
     * @param paid Boolean indicating if the bounty has been paid out
     * @param timestamp Time when the bounty was claimed
     */
    struct Claim {
        address developer;
        uint256 amount;
        bool claimed;
        bool paid;
        uint256 timestamp;
    }

    // Reference to the FundManager contract that manages bounty funds
    IFundManager private immutable _fundManager;

    // Reference to the validator multisig contract that approves claims
    IValidatorMultiSig public validatorMultiSig;

    // Address of the contract owner who can update validator address
    address public owner;

    // Mapping from issue ID to claim details
    mapping(uint256 => Claim) public claims;

    // Time window during which a claim must be processed (7 days)
    uint256 public constant CLAIM_TIMEOUT = 7 days;

    /**
     * @dev Restricts function access to the contract owner
     */
    modifier onlyOwner() {
        require(msg.sender == owner, "Only owner can call this function");
        _;
    }

    /**
     * @dev Constructor to initialize the contract
     * @param _fundManagerAddr Address of the FundManager contract
     * @param _validatorMultiSig Address of the ValidatorMultiSig contract
     */
    constructor(address _fundManagerAddr, address _validatorMultiSig) {
        require(_fundManagerAddr != address(0), "Invalid fund manager address");
        require(_validatorMultiSig != address(0), "Invalid validator address");
        _fundManager = IFundManager(_fundManagerAddr);
        validatorMultiSig = IValidatorMultiSig(_validatorMultiSig);
        owner = msg.sender;
    }

    /**
     * @dev Returns the address of the FundManager contract
     * @return Address of the FundManager contract
     */
    function fundManager() external view override returns (address) {
        return address(_fundManager);
    }

    /**
     * @dev Updates the validator multisig contract address
     * @param _newValidator New validator multisig contract address
     */
    function updateValidator(address _newValidator) external onlyOwner {
        require(_newValidator != address(0), "Invalid validator address");
        validatorMultiSig = IValidatorMultiSig(_newValidator);
    }

    /**
     * @dev Ensures the bounty for the given issue exists, is active, and hasn't been claimed
     * @param issueId ID of the issue with the bounty
     */
    modifier onlyUnclaimedBounty(uint256 issueId) {
        uint256 bountyAmount = _fundManager.getIssueBounty(issueId);
        require(bountyAmount > 0, "No bounty for this issue");

        (,,, bool active) = _fundManager.getBountyDetails(issueId);
        require(active, "Bounty is not active");
        require(!claims[issueId].claimed, "Bounty already claimed");
        _;
    }

    /**
     * @dev Allows a developer to claim a bounty for a resolved issue
     * @param issueId ID of the issue with the bounty
     * Emits a BountyClaimed event on success
     */
    function claimBounty(uint256 issueId) external override onlyUnclaimedBounty(issueId) {
        uint256 bountyAmount = _fundManager.getIssueBounty(issueId);

        claims[issueId] = Claim({
            developer: msg.sender,
            amount: bountyAmount,
            claimed: true,
            paid: false,
            timestamp: block.timestamp
        });

        emit BountyClaimed(issueId, msg.sender);
    }

    /**
     * @dev Processes the payout for a claimed bounty after validator approval
     * @param issueId ID of the issue with the claimed bounty
     * Emits a PayoutProcessed event on successful payout
     */
    function processPayout(uint256 issueId) external override {
        Claim storage claim = claims[issueId];
        require(claim.claimed, "Bounty not claimed");
        require(!claim.paid, "Bounty already paid");
        require(block.timestamp <= claim.timestamp + CLAIM_TIMEOUT, "Claim expired");

        require(validatorMultiSig.isApproved(issueId, claim.developer),
            "Not approved by validators");

        claim.paid = true;

        (bool success, ) = payable(claim.developer).call{value: claim.amount}("");
        require(success, "Transfer failed");

        emit PayoutProcessed(issueId, claim.developer, claim.amount);
    }

    /**
     * @dev Returns the claim status for a given issue ID
     * @param issueId ID of the issue to check
     * @return claimed Boolean indicating if the bounty has been claimed
     * @return developer Address of the developer who claimed the bounty
     * @return amount The amount of the bounty in wei
     */
    function getClaimStatus(uint256 issueId)
    external
    view
    override
    returns (bool claimed, address developer, uint256 amount)
    {
        Claim storage claim = claims[issueId];
        return (claim.claimed, claim.developer, claim.amount);
    }

    /**
     * @dev Allows a developer to cancel their claim on a bounty
     * @param issueId ID of the issue with the claimed bounty
     * Emits a ClaimCancelled event on successful cancellation
     */
    function cancelClaim(uint256 issueId) external override {
        Claim storage claim = claims[issueId];
        require(claim.claimed, "Bounty not claimed");
        require(!claim.paid, "Bounty already paid");
        require(claim.developer == msg.sender, "Only claimer can cancel");

        delete claims[issueId];

        emit ClaimCancelled(issueId, msg.sender);
    }

    /**
     * @dev Fallback function to receive ETH payments
     */
    receive() external payable {}
}