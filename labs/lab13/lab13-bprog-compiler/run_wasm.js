// run_wasm.js - Script to run compiled BPROG WebAssembly programs
const fs = require('fs');
const path = require('path');

async function runWasm(wasmPath) {
  try {
    console.log(`Loading WebAssembly module: ${wasmPath}`);
    const wasmBuffer = fs.readFileSync(wasmPath);
    
    // Define runtime imports needed by the WebAssembly module
    const imports = {
      env: {
        // Memory (can be imported or exported by the module)
        memory: new WebAssembly.Memory({ initial: 1 }),
        
        // I/O functions
        print_i32: (value) => {
          console.log(`Integer: ${value}`);
        },
        print_f32: (value) => {
          console.log(`Float: ${value}`);
        },
        print_string: (ptr) => {
          // Read null-terminated string from memory
          const memory = imports.env.memory.buffer;
          const view = new Uint8Array(memory);
          let str = '';
          let i = ptr;
          while (view[i] !== 0) {
            str += String.fromCharCode(view[i]);
            i++;
          }
          console.log(`String: "${str}"`);
        },
        read_line: () => {
          // Not implemented in this simple example
          // In a real implementation, this would read from stdin
          console.log('[read_line called - returning empty string]');
          return 0; // Return pointer to empty string
        }
      }
    };
    
    // Instantiate the WebAssembly module
    console.log('Instantiating WebAssembly module...');
    const { instance } = await WebAssembly.instantiate(wasmBuffer, imports);
    
    // Check if the module exports memory
    if (instance.exports.memory) {
      imports.env.memory = instance.exports.memory;
      console.log('Using memory exported by the module');
    }
    
    // Execute the main function
    console.log('Executing main function...');
    const result = instance.exports.main();
    console.log(`\nExecution complete`);
    console.log(`Result: ${result}`);
    
    // For debugging: list all exports
    console.log('\nModule exports:');
    for (const exportName in instance.exports) {
      const exportType = typeof instance.exports[exportName];
      console.log(` - ${exportName}: ${exportType}`);
    }
    
  } catch (error) {
    console.error('Error running WebAssembly module:');
    console.error(error);
  }
}

// Get the WASM file path from command line arguments
if (process.argv.length < 3) {
  console.error('Please provide a WASM file path');
  console.error('Usage: node run_wasm.js <path-to-wasm-file>');
  process.exit(1);
}

// Run the WebAssembly module
const wasmPath = process.argv[2];
runWasm(wasmPath);