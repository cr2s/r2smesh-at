#!/bin/bash

# User parameters for R2S are supplied as environment variables.
# All variables relevant for R2S start with "r2s_".

# On the forHLR-2 cluster, where fispact-driver was 
# developed, these modules were used for all mcnp and
# for the fispact-driver
module load compiler/intel/17.0 mpi/impi/2017

# Path to the folder containing fispact-II data and to the
# fispact executable. These environment variables are used
# in the scripts condense.sh, collapse.sh and inventory.sh.
export r2s_fsp_data=$PROJECT/fispact-II
export r2s_fsp_exe=`which fispact-II`

# Fispact working directory. All fispact working directories will be created
# under this folder
export r2s_fwd="fispact_wd"
mkdir -p "$r2s_fwd/col"
mkdir -p "$r2s_fwd/inv"
mkdir -p "$r2s_fwd/gi"

# Place for scratch files.  Must be existing folder!
export r2s_scratch=$(realpath ./scratch)
# export r2s_scratch=$SCRATCH/r2s
mkdir -p "$r2s_scratch"

# SPlit the inventory input into the header and footer
csplit -s inv_input.template '/<< Irradiation Scenario/' 
mv xx00 inv_input.header
mv xx01 inv_input.footer


