#!/bin/fish
fd . tests -e jin -d 1 | sed -E 's/tests\/(.*)\.jin/\1/' | xargs -L 1 ./run_test.fish
