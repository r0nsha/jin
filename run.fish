#!/bin/fish
argparse --name=run --min-args=1 r/release d/debug -- $argv
or exit 1

set -l source $argv[1]

set -l cargo_flags
set -l build_flags
set -l exedir

if set -q _flag_r
    set cargo_flags -r
    set exedir target/release
else
    set exedir target/debug
end

if set -q _flag_d
    set build_flags --timings --emit ast --emit hir --emit mir --emit c
end

set -l source $argv[1]
set -l source_dir (dirname $source)
set -l output_name (basename $source .jin)
set -l output $source_dir/build/$output_name

# Copy std package
rm -r $exedir/std
cp -r std $exedir/std

# Build runtime
cd rt
zig build
cd ..

# Copy runtime library & header
set -l rtdir $exedir/rt
rm -r $rtdir
mkdir $rtdir &>/dev/null
cp rt/jinrt.h $rtdir/jinrt.h
cp rt/zig-out/lib/libjinrt.a $rtdir/libjinrt.a

# Run compiler
cargo run $cargo_flags -- build $source $build_flags

# Run output file if exists
if test $status -eq 0 && test -f $output
    $output
end
