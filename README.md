# Jin

Jin is a statically typed, procedural and safe programming language.

## Highlights

- Deterministic, automatic memory management
- Pattern matching and union types
- Polymorphic types
- Compiled

## Hello, World

```
fn main() = {
    println("Hello, World!")
}
```

## Goals

- Fast enough for use-cases such as servers and cli applications
- Simple and orthogonal
- Fast compile times

## Non-goals

- Competing with low-level languages like Rust and Zig.
- Simple (small surface area)
- Fast compile times

## Usage

```sh
jin build main.jin
./main
```
