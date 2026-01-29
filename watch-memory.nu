#!/bin/env nu
# This is a nu shell script to check the memory consumption of the program.
# This just for checking when WSL crashes.

loop {
    let x = ps
        | sort-by mem --reverse
        | where name == "lens-sl"
        | get mem;
    let msg = if $x == [] { echo "0B" } else { $x | get 0 }
    print -n $"Memory Usage: ($msg)        \r"
    sleep 0.2sec
}
