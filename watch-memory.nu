#!/bin/env nu
# This is a nu shell script to check the memory consumption of the program.
# This just for checking when WSL crashes.

loop {
    let x = ps
        | sort-by mem --reverse
        # | where name in [lens-sl, cvc5]
        | where name in [lens-sl]
        | group-by name
        | update cells { reject name ppid virtual }
        | get lens-sl
    clear
    $x | table --expand -d 5 --theme none | print
    sleep 0.2sec
}
