# Jin

Jin is a work-in-progress programming language, and is intended to be a general-purpose, compiled and simple.

## Hello, World

```
fn main() unit = {
    println("Hello, World!")
}
```

## Goals

- Fast enough for use-cases such as servers and cli applications
- Simple (small surface area)
- Fast compile times

## Usage

```sh
jin build main.jin
./main
```
