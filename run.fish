#!/bin/fish
cargo run -- build tests/demo.jin --timings --emit hir --emit mir --emit llvm-ir && tests/build/demo
