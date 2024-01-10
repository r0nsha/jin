#!/bin/fish
if set -q $argv[1]
    echo Usage: run.fish [FILE_NAME]
else
    set -l source $argv[1]
    set -l source_dir (dirname $source)
    set -l output_name (basename $source .jin)
    set -l output $source_dir/build/$output_name

    cargo run -- build $source --timings --emit ast --emit hir --emit mir --emit c

    if test $status -eq 0 && test -f $output
        cp -r std target/std
        $output
    end
end
