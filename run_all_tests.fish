#!/bin/fish
fd . tests -e jin -d 1 | xargs -L 1 ./run.fish
