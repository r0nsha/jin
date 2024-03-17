#!/bin/fish
cargo build -r

time for file in (fd . tests -e jin -d 1)
    echo (set_color -o brgreen)Running (set_color normal)$file
    target/release/jin run $file -o build
    echo
end

rm -r tests/build
