#!/bin/bash

# Script creates folder for inventory fispact run.
# Input file is constructed by the driver.

wdr="$r2s_scratch"/r2s_i/$1/$2/$3        # Inventory working folder
cld="$r2s_scratch"/r2s_c/$4/$5/$6        # Collapse working folder
scr="$r2s_scratch"/r2s_r                 # Scratch foder, where resulting tab4 files are moved
scw="$r2s_scratch"/r2s_w                 # Scratch foder, where parts of the inventory input are written by the driver 
org=$(pwd)


# Do something only when corresponding tab4 does not exist
if [ -a "$scr"/tab4.$1.$2.$3 ]; then 
   exit 0
fi   


# Prepare working directory, if not exists
if [ ! -d "$wdr" ]; then

    mkdir -p "$wdr"
    # Links to files and fispact data folder
    ln -s "$scr"/inventory_files      "$wdr"/files
    ln -s "$scr"/fispact-data         "$wdr"/fispact-data

    # Links to condense and collapse results
    ln -s "$cld"/collapx   "$wdr"/collapx
    ln -s "$scr"/arrayx    "$wdr"/arrayx

    # Concatenate parts of the input file
    cat "$scr"/header     \
        "$scw"/mat.title.$1.$2.$3 \
        "$scw"/mat.content.$1.$2.$3 \
        "$scw"/scenario.$1.$2.$3 > "$wdr"/inventory.i
fi

# Execute fispact if inventory.tab4 not exists
if [ ! -f "$wdr"/inventory.tab4 ]; then
    cd "$wdr"

    # Execute fispact and exit if fispact exit status was non-zero
    "$r2S_fispact_exe" inventory > fispact.out  || exit 1
    cd "$org"
fi    
mv "$wdr"/inventory.tab4 "$scr"/tab4.$1.$2.$3


exit 0
