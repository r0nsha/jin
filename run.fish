#!/bin/fish
if set -q $argv[1]
    echo Usage: run.fish [FILE_NAME]
else
    set -l name $argv[1]
    set -l src tests/$name.jin
    set -l out tests/build/$name

    cargo run -- build $src --timings --emit hir --emit tir --emit llvm-ir

    if test $status -eq 0 && test -f $out
        $out
    end
end
