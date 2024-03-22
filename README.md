# Jin

Jin is a statically typed, procedural and safe programming language.

## Hello, World

```
fn main() unit = {
    println("Hello, World!")
}
```

## Highlights

- Automatic, deterministic memory management
- Pattern matching and tagged union types
- Parametric polymorphism and distinct typing
- Local type inference with required top level type annotations
- Compiled to machine code (Currently compile to C)

## Goals

- Simple enough that it can be learned in an afternoon
- Fast compile times
- Performance that statisfies use-cases such as servers and applications

## Prerequisites

- You'll need to install [Zig](https://ziglang.org/) on your machine, since we use `zig cc` to compile the generated C code.

## Usage

```sh
jin build main.jin
./main
```

## Building from source

To build from source, you'll need [Rust](https://rust-lang.org/) to build the compiler and [Zig](https://ziglang.org/) for compiling the runtime (which is done automatically when building the compiler).
After both are installed, you can `cd` to the repository and run `cargo build -r` (remove the `-r` for a debug build). The compiler artifacts will be under `target/release` or `target/debug`, depending on the mode you built the compiler in.

The artifacts you'll need to run the compiler are:

- `jin` - The compiler binary
- `std/` - The standard library
- `rt/` - The runtime library

**It's important that both std/ and rt/ are in the same directory as the binary, as that is where it searches them!**
