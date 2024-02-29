#!/bin/fish
cargo build

time for file in (fd . examples -e jin -d 1)
    echo (set_color -o brgreen)Running (set_color normal)$file
    target/debug/jin run $file -o build
    echo
end

rm -r examples/build
