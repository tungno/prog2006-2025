#!/bin/bash
# Debug script for BPROG interpreter

echo "Starting BPROG REPL Debug Session"
echo "Type expressions to evaluate"
echo "Enter 'quit' to exit"
echo ""

while true; do
  echo -n "bprog-debug> "
  read line
  
  if [ "$line" = "quit" ]; then
    break
  fi
  
  # Create a temporary file with the input
  echo "$line" > temp.bprog
  
  # Run the interpreter
  echo "Running: $line"
  stack run temp.bprog
  
  # Remove the temporary file
  rm temp.bprog
  
  echo ""
done

echo "Debug session ended"