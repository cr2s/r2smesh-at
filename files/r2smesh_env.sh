#!/bin/bash

# User parameters for R2S are supplied as environment variables.
# All variables relevant for R2S start with "r2s_".

. ./modules.sh

# Path to the folder containing fispact-II data and to the
# fispact executable. These environment variables are used
# in the scripts condense.sh, collapse.sh and inventory.sh.
export r2s_fsp_data=/marconi_work/FUA11_MCHIFI/atr_share/fispact-II
export r2s_fsp_exe=`which fispact-II`

# Fispact working directory. All fispact working directories will be created
# under this folder
export r2s_fwd="$(realpath fispact_wd)"

# Place for scratch files.  Must be existing folder!
export r2s_scratch="$(realpath ./scratch)"
# export r2s_scratch=$SCRATCH/r2s
