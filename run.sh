#!/usr/bin/env bash
cargo run -- build tests/simple.un && out/main | echo $?
