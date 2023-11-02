#!/bin/fish
if set -q $argv[1]
    echo Usage: run.fish [FILE_NAME]
else
    set -l name $argv[1]
    set -l src tests/$name.jin
    set -l out tests/build/$name.c

    cargo run -- build $src --timings --emit hir --emit mir --emit c

    cat $out
end
