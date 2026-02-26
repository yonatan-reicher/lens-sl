#!/bin/env nu
# This is a nu shell script to check the memory consumption of the program.
# This just for checking when WSL crashes.

loop {
    let x = ps
        | sort-by mem --reverse
        | where name in [lens-sl, cvc5]
        | group-by name
        | update cells { reject name ppid virtual }
    clear
    $x | table --expand | print
    sleep 0.2sec
}
