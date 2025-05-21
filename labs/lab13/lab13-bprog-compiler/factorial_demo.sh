#!/bin/bash
# factorial_demo.sh - Script to demonstrate factorial in WebAssembly

FACTORIAL_WAT="factorial_template.wat"
FACTORIAL_WASM="factorial.wasm"

echo "==== Demonstrating factorial in WebAssembly ===="
echo "BPROG SOURCE CODE:"
cat examples/factorial.bprog
echo ""

echo "==== Using fixed factorial template for demonstration ===="

echo "==== Converting WAT to WASM ===="
# Convert WAT to WASM
wat2wasm "$FACTORIAL_WAT" -o "$FACTORIAL_WASM"

if [ $? -ne 0 ]; then
    echo "Error converting WAT to WASM"
    exit 1
fi

echo "WebAssembly binary output saved to $FACTORIAL_WASM"

# Check if Node.js is installed
if ! command -v node &> /dev/null; then
    echo "Node.js not found. Please install Node.js to run the WebAssembly module"
    exit 1
fi

echo "==== Running WebAssembly module ===="
# Run the WASM file with Node.js
node --experimental-wasm-modules run_wasm.js "$FACTORIAL_WASM"

echo "==== Done ===="