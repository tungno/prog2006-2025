# Lab 10: Snake Game

* Deadline (hard): 11th of April, 10:15, paper check-in
* Score: 7, C-level task
* Language: Rust or Haskell
* LLMs: **use them**
* Co-pilots: **use them**


## Overview

In this lab, you will implement the classic Snake game using:
* Rust and the `Iced.rs` GUI framework. Or,
* Haskell, and a framework of your own choice.

This project will help you develop your skills in Rust/Haskell programming, GUI development, game logic implementation, and state management.


## Learning Objectives

- Gain practical, real-world experience with the Rust/Haskell programming language
- Learn to use the Iced framework for creating graphical user interfaces (Rust) or any framework of your choice in Haskell.
- Implement game mechanics and collision detection
- Manage application state in a functional reactive paradigm
- Create both single-player and multiplayer game modes


## Game Requirements

### Core Functionality
1. Implement a classic Snake game with the following features:
   - A game grid (minimum 20x20)
   - Snake movement in four directions (up, down, left, right)
   - Food/coin generation at random positions
   - Score tracking
   - Game over detection (collision with walls or self)
   - Visual display of the game state

### Single-Player Mode
1. The player controls a snake that starts with a length of 3 segments
2. The snake moves continuously in the current direction
3. Player can change direction using keyboard input (arrow keys or WASD)
4. When the snake eats food/coins:
   - The snake grows by one segment
   - The score increases
   - New food appears at a random empty position
5. The game ends when:
   - The snake collides with a wall (if wall collision is enabled)
   - The snake collides with itself
6. Display the current score and high score
7. Implement a "Game Over" screen with an option to restart

### Two-Player Mode
1. Two snakes on the same grid, each controlled by a different player
2. Player 1 uses WASD keys, Player 2 uses arrow keys
3. Each snake has a different color
4. Both snakes compete for the same food items
5. Additional collision rule: game ends for a player if their snake collides with the other player's snake
6. Display separate scores for each player
7. When one player loses, indicate the winner and provide an option to restart


## Technical Requirements

### Haskell

1. Any GUI or Text based terminal framework is fine. I had success with SDL2 and GTK4 bindings, but recent search also recommends Gloss for GUI and Brick for text based support. Try what works for you. GUI Options:
   - Gloss - A vector graphics library ideal for 2D games
   - SDL2 (via Haskell bindings) - For hardware-accelerated graphics
   - GTK (via Haskell bindings) - For a widget-based interface
   - Brick if you want to stick to terminal and text-based UI
2. Organize your code using appropriate functional programming patterns and practices
3. Implement proper error handling using Either, Maybe, or similar approaches
4. Make effective use of Haskell's type system and create appropriate data types for game entities
5. Utilize pure functions where possible and contain side effects appropriately
6. Implement game logic in a functional style

### Rust 
1. Use `Iced.rs`. Do not use any other GUI framework
2. Organize your code using appropriate Rust patterns and practices
3. Implement proper error handling
4. You can use additional crates to help you.
5. Utilize Iced's reactive model for state management
6. Use Iced's Canvas for custom drawing operations


### Project Structure
Your project should include:
- A well-structured main module
- Separate modules for game logic, UI rendering, and state management
- Clear separation between single-player and multiplayer functionality
- Well-documented code with comments explaining key algorithms and design decisions


## Resources
- [Gloss](https://hackage.haskell.org/package/gloss), GTK bindings or [SDL2](https://hackage.haskell.org/package/sdl2). Ask in `#haskell` before you try a given dependency to make sure it works.
- Iced documentation: https://docs.rs/iced/latest/iced/
- Game development patterns and best practices
- Snake game mechanics reference

Good luck, and enjoy building your Snake game!