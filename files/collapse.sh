#!/bin/bash

# Script creates folder for collapse fispact run and starts fispact there.
# The file with spectrum is written by the driver to r2s_scratch/fluxes_i_j_k. 

wdr="$r2s_fwd"/col/$1/$2/$3   # fispact working folder
sdr="$r2s_scratch"            # scratch folder
cdr="$(pwd)"                  # current folder (where adriver starts)

# If collapx file exists, do nothing
if [ -f "$wdr"/collapx ]; then 
    exit 0
fi    

# Create fispact working folder, if necessary
if [ ! -d "$wdr" ]; then 
    mkdir -p "$wdr"
    ln -s "$cdr"/clp_files.template "$wdr"/files
    ln -s "$cdr"/clp_input.template "$wdr"/collapse.i
    ln -s "$r2s_fsp_data"           "$wdr"/fispact-data
    ln -s "$sdr"/fluxes.$1.$2.$3    "$wdr"/fluxes
fi

# Run fispact
cd "$wdr"
"$r2s_fsp_exe" collapse > fispact.out || exit 1

# Remove auxiliary files when fispact run was successful
# rm files collapse.i fispact-data fluxes fispact.out collapse.log collapse.out
# rm "$sdr"/fluxes.$1.$2.$3
# cd "$cdr"

exit 0


