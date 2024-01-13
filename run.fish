#!/bin/fish
if set -q $argv[1]
    echo Usage: run.fish [FILE_NAME]
else
    set -l source $argv[1]
    set -l source_dir (dirname $source)
    set -l output_name (basename $source .jin)
    set -l output $source_dir/build/$output_name

    # Copy std package
    cp -r std target/std

    # Copy runtime
    mkdir target/rt
    cp rt/jinrt.h target/rt/jinrt.h

    cargo run -- build $source --timings --emit ast --emit hir --emit mir --emit c

    if test $status -eq 0 && test -f $output
        $output
    end
end
