#!/bin/bash

# Script creates folder for collapse fispact run and starts fispact there.
# The file with spectrum is written by the driver to r2s_scratch/fluxes_i_j_k. 

d="$r2s_fwd/col/$1/$2/$3"

if [ ! -d "$d" ]; then 
    o=$(pwd)
    mkdir -p "$d"
    ln -s $o/clp_files.template "$d"/files
    ln -s $o/clp_input.template "$d"/collapse.i
    ln -s $r2s_fsp_data "$d"/fispact-data
    mv "$r2s_scratch/fluxes.$1.$2.$3" "$d"/fluxes
    o="$(pwd)"
    cd "$d"
    if [ ! -f collapx ]; then 
        "$r2s_fsp_exe" collapse > fispact.out
    fi    
    cd "$o"
fi    
exit 1


