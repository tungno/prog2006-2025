// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

/*
    Rock Paper Scissors Game

    Game rules:
      - Paper (P) beats Rock (R)
      - Rock (R) beats Scissors (S)
      - Scissors (S) beats Paper (P)
      
    Protocol overview:
      1. Both players “commit” to a move by sending a hash of their chosen move (R,P, or S + secret salt).
         This is done in the `play` function.
      2. Both players later “reveal” their move via the `reveal` function, providing the same salt.
      3. The contract compares moves, determines the winner, and the winner can withdraw the pot.
         In a tie, each player withdraws their own bet.
      4. If one player fails to reveal in time, the other can withdraw the entire pot.
*/

contract RockPaperScissors {
    // --- EVENTS ---
    event Played(address indexed player, bytes32 hashedVote);
    event Revealed(address indexed player, string vote);
    event Withdrawn(address indexed player, uint256 amount);

    // --- CONSTANTS ---
    uint256 public constant revealTimeout = 5 minutes;

    // --- GAME STATE VARIABLES ---
    address public player1;
    address public player2;
    uint256 public bet;            // The stake; both players must send the same amount
    bytes32 public commit1;        // Player1’s hashed move
    bytes32 public commit2;        // Player2’s hashed move
    bool public revealed1;         // Whether player1 has revealed
    bool public revealed2;         // Whether player2 has revealed
    string public move1;           // Player1’s revealed move
    string public move2;           // Player2’s revealed move
    uint256 public gameStartTime;  // Timestamp when second player joined (to track reveal timeout)
    bool public gameCompleted;     // Flag set when game is finalized
    address public winner;         // Winner’s address
    bool public tie;               // True if game ended in a tie

    // Pending withdrawals
    mapping(address => uint256) public pendingWithdrawals;

    // --- play ---
    function play(bytes32 _commitment) external payable {
        require(msg.value > 0, "Stake must be non-zero");

        // If no player1, set the first caller as player1
        if (player1 == address(0)) {
            player1 = msg.sender;
            commit1 = _commitment;
            bet = msg.value;
            emit Played(msg.sender, _commitment);
        } 
        // Else, we set the second caller as player2
        else {
            require(player2 == address(0), "Game already has two players");
            require(msg.sender != player1, "Cannot play against yourself");
            require(msg.value == bet, "Stake must match the first player's stake");

            player2 = msg.sender;
            commit2 = _commitment;
            gameStartTime = block.timestamp;
            emit Played(msg.sender, _commitment);
        }
    }

    // --- reveal ---
    function reveal(string calldata _move, string calldata _salt) external {
        require(msg.sender == player1 || msg.sender == player2, "Not a participant");
        bytes memory moveBytes = bytes(_move);
        require(moveBytes.length == 1, "Move must be R, P, or S");
        bytes1 moveChar = moveBytes[0];
        require(
            moveChar == bytes1("R") || moveChar == bytes1("P") || moveChar == bytes1("S"),
            "Invalid move. Use 'R','P','S'"
        );

        // Recompute hash
        bytes32 computedHash = keccak256(abi.encodePacked(_move, _salt));

        // Reveal for player1
        if (msg.sender == player1) {
            require(!revealed1, "Player1 already revealed");
            require(computedHash == commit1, "Invalid reveal for player1");
            revealed1 = true;
            move1 = _move;
            emit Revealed(msg.sender, _move);
        }
        // Reveal for player2
        else {
            require(!revealed2, "Player2 already revealed");
            require(computedHash == commit2, "Invalid reveal for player2");
            revealed2 = true;
            move2 = _move;
            emit Revealed(msg.sender, _move);
        }

        // If both have revealed, determine the result
        if (revealed1 && revealed2) {
            _determineWinner();
        }
    }

    // --- INTERNAL: _determineWinner ---
    function _determineWinner() internal {
        if (bytes(move1)[0] == bytes(move2)[0]) {
            // Tie
            tie = true;
            gameCompleted = true;
            pendingWithdrawals[player1] = bet;
            pendingWithdrawals[player2] = bet;
        } else {
            // R beats S, P beats R, S beats P
            bool p1Wins = (
                (bytes(move1)[0] == "R" && bytes(move2)[0] == "S") ||
                (bytes(move1)[0] == "P" && bytes(move2)[0] == "R") ||
                (bytes(move1)[0] == "S" && bytes(move2)[0] == "P")
            );

            winner = p1Wins ? player1 : player2;
            gameCompleted = true;
            pendingWithdrawals[winner] = bet * 2;
        }
    }

    // --- withdraw ---
    function withdraw() external {
        // If the game hasn't properly concluded, check timeout logic
        if (!gameCompleted && player2 != address(0)) {
            // If time is up
            if (block.timestamp >= gameStartTime + revealTimeout) {
                if (revealed1 && !revealed2) {
                    // player1 revealed, player2 didn't -> player1 wins
                    winner = player1;
                    gameCompleted = true;
                    pendingWithdrawals[player1] = bet * 2;
                } else if (revealed2 && !revealed1) {
                    // player2 revealed, player1 didn't -> player2 wins
                    winner = player2;
                    gameCompleted = true;
                    pendingWithdrawals[player2] = bet * 2;
                } else {
                    revert("No reveal by any player; cannot withdraw");
                }
            } else {
                revert("Game not completed and timeout not reached");
            }
        }

        uint256 amount = pendingWithdrawals[msg.sender];
        require(amount > 0, "No funds to withdraw");

        // Prevent re-entrancy
        pendingWithdrawals[msg.sender] = 0;

        (bool success, ) = msg.sender.call{value: amount}("");
        require(success, "Transfer failed");

        emit Withdrawn(msg.sender, amount);

        // Reset the game if everyone’s funds are withdrawn
        if (gameCompleted) {
            if (pendingWithdrawals[player1] == 0 && pendingWithdrawals[player2] == 0) {
                _resetGame();
            }
        }
    }

    // --- INTERNAL: _resetGame ---
    function _resetGame() internal {
        player1 = address(0);
        player2 = address(0);
        bet = 0;
        commit1 = 0;
        commit2 = 0;
        revealed1 = false;
        revealed2 = false;
        move1 = "";
        move2 = "";
        gameStartTime = 0;
        gameCompleted = false;
        winner = address(0);
        tie = false;
    }

    // Allow direct ETH transfers if desired
    receive() external payable {}
}
