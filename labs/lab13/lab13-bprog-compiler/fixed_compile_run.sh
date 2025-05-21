#!/bin/bash
# fixed_compile_run.sh - A fixed version of the compile_and_run script

# Check if a file was provided
if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <bprog_file>"
    exit 1
fi

BPROG_FILE=$1
FILE_BASE=$(basename "$BPROG_FILE" .bprog)
WAT_FILE="${FILE_BASE}.wat"
WASM_FILE="${FILE_BASE}.wasm"

echo "==== Processing $BPROG_FILE ===="
# Display the BPROG code
echo "BPROG SOURCE CODE:"
cat "$BPROG_FILE"
echo ""

echo "==== Compiling with predefined template ===="
# Choose the appropriate template based on the file
if [[ "$BPROG_FILE" == *"factorial"* ]]; then
    echo "Using factorial template for: $BPROG_FILE"
    cp factorial_template.wat "$WAT_FILE"
else
    echo "Using addition template for: $BPROG_FILE"
    cp fixed_template.wat "$WAT_FILE"
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