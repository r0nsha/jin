#!/bin/fish
if set -q $argv[1]
    echo Usage: run.fish [FILE_NAME]
else
    set -l name $argv[1]
    set -l src examples/$name.jin
    set -l out examples/build/$name

    cargo run -- build $src --timings --emit ast --emit hir --emit mir --emit c

    if test $status -eq 0 && test -f $out
        cp -r std target/std
        $out
    end
end
