#!/bin/bash

# Script creates folder for condense fispact run.

d="$(realpath ./cond)"
o=$(pwd)

if [ ! -d "$d" ]; then
    mkdir -p "$d"
fi     

if [ ! -f "$d/arrayx" ]; then
    ln -s $r2S_condense_files "$d"/files
    ln -s $r2S_condense_input "$d"/condense.i
    ln -s $r2S_fispact_data   "$d"/fispact-data

    cd "$d"
    "$r2S_fispact_exe" condense > fispact.out
    exit
fi
exit
