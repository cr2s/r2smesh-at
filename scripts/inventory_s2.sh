#!/bin/bash

# Script cleans inventory fispact run.

wdr="$r2s_scratch"/r2s_i/$1/$2/$3        # Inventory working folder
scr="$r2s_scratch"/r2s_r                 # Scratch foder, where resulting tab4 files are moved
scw="$r2s_scratch"/r2s_w                 # Scratch foder, where parts of the inventory input are written by the driver 


rm "$scr"/tab4.$1.$2.$3
rm -rf "$wdr"
rm "$scw"/mat.title.$1.$2.$3 \
   "$scw"/mat.content.$1.$2.$3 \
   "$scw"/scenario.$1.$2.$3 


exit 0

