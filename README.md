# Compiler
So far, there's no name, but I'll find one eventually.

## Installation
This project requires LLVM 14 to be installed and available in your PATH.

To install LLVM, you can use:
```bash
sudo apt-get update && sudo apt-get install -y llvm-14 llvm-14-dev libpolly-14-dev
```

You may need to export the LLVM path for llvm-sys:
```bash
export LLVM_SYS_140_PREFIX=/usr/lib/llvm-14
```

## Compilation
To compile a file, use this command:
```bash
cargo run --release -- path/to/your/file.lang
```

## Future Plans
The lexer and the parser are pretty much in their final forms and won't be changed too much in future. This is what will be changed in future:
- Complete typechecker rewrite to add passes, and improve memory management support.
- Implement array types and expose pointer arithmetic.
- Expose external functions to users.
- Add more standard library functions.

