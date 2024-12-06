#!/bin/bash

d2=$(printf "%02d" $1)
name=dec$d2

stack build --file-watch --test --exec "bash -c \"stack exec $name-run < ../inputs/2024/$d2.txt\""
