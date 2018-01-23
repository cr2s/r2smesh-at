#!/bin/bash

# Script creates folder for collapse fispact run and starts fispact there.
# The file with spectrum is written by the driver to r2s_scratch/fluxes_i_j_k. 

wdr="$r2s_fwd"/col/$1/$2/$3   # fispact working folder
sdr="$r2s_scratch"            # scratch folder

# Remove collapse run (called after finished all inventory calculations for the coarse mesh element) 
rm -rf "$wdr"
rm "$sdr"/fluxes.$1.$2.$3

exit 0



