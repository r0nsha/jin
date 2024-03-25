#!/bin/fish
cargo build -r

time for dir in (fd . tests -d 1)
    set -l file $dir/main.jin
    echo (set_color -o brgreen)Running (set_color normal)$file
    target/release/jin run $file -o test-out
    rm -r $dir/test-out
    echo
end

rm -r tests/build
