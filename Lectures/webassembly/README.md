# WASM and tooling

* **WASI**: To LLVM/Clang compile WASM with WASI, you need WASI SDK installed.
* **wat2wasi**: Compiling WAT to WASM
* **wasm-dis**: WASM disassembler, generates WAT representation


# Some short commands for WASM

* Building for embedable module
```bash
clang --target=wasm32 -nostdlib -Wl,--no-entry  -Wl,--export-all -o add.wasm add.c
```
* Building for executable WASM with WASI functions
```bash
clang --target=wasm32-wasi -o hello.wasm hello.c
```
* Building Go to WASM
```bash
GOOS=wasip1 GOARCH=wasm go build -o main.wasm main.go
```

