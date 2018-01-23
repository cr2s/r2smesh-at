#!/bin/bash

# Script creates folder $1 for inventory fispact run.
# The collapsed xs are taken from the $2 folder.
# Input file is constructed by the driver.

wdr="$r2s_fwd"/inv/$1/$2/$3   # fispact working folder
sdr="$r2s_scratch"            # scratch folder

# remove all inventory-related files (called after reading the tab4 file) 
# rm "$sdr"/tab4.$1.$2.$3
rm "$sdr"/mat.title.$1.$2.$3
rm "$sdr"/mat.content.$1.$2.$3
rm "$sdr"/scenario.$1.$2.$3
rm -rf "$wdr"

exit 0

