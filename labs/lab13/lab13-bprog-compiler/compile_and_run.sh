#!/bin/bash
# compile_and_run.sh - Script to compile BPROG to WASM and run it

# Check if a file was provided
if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <bprog_file>"
    exit 1
fi

BPROG_FILE=$1
FILE_BASE=$(basename "$BPROG_FILE" .bprog)
WAT_FILE="${FILE_BASE}.wat"
WASM_FILE="${FILE_BASE}.wasm"

echo "==== Compiling $BPROG_FILE to WebAssembly ===="
# Compile BPROG to WAT
stack exec lab13-bprog-compiler -- -c "$BPROG_FILE" > "$WAT_FILE"

if [ $? -ne 0 ]; then
    echo "Error compiling BPROG to WAT"
    exit 1
fi

echo "WebAssembly text output saved to $WAT_FILE"

# Check if wat2wasm is installed
if ! command -v wat2wasm &> /dev/null; then
    echo "wat2wasm not found. Please install WABT (WebAssembly Binary Toolkit)"
    echo "  macOS: brew install wabt"
    echo "  Linux: apt-get install wabt"
    exit 1
fi

echo "==== Converting WAT to WASM ===="
# Convert WAT to WASM
wat2wasm "$WAT_FILE" -o "$WASM_FILE"

if [ $? -ne 0 ]; then
    echo "Error converting WAT to WASM"
    exit 1
fi

echo "WebAssembly binary output saved to $WASM_FILE"

# Check if Node.js is installed
if ! command -v node &> /dev/null; then
    echo "Node.js not found. Please install Node.js to run the WebAssembly module"
    exit 1
fi

echo "==== Running WebAssembly module ===="
# Run the WASM file with Node.js
node --experimental-wasm-modules run_wasm.js "$WASM_FILE"

echo "==== Done ===="