#!/bin/bash

# Setup environment variables for r2s
# Variables r2s_* are used only here to define common parts of the other variables.
# Variables r2S_* are used in the scripts only (not used in the driver).
# Variables R2s_* are used in the driver (may or not be used in scripts)

# USAGE NOTE
# This script should be sourced (not executed) to make all the variables defined here become available
# in the current shell. I.e, 
#              > . ./r2s_env.sh      # this is correct
#              > ./r2s_env.sh        # this is wrong
# For details see e.g. 
# https://superuser.com/questions/176783/what-is-the-difference-between-executing-a-bash-script-vs-sourcing-it 

# Try to continue? (Existing out folder will not be moved to out.old.N)
export r2S_continue=no  # any other value means "no".

# Root folder for the r2s distribution.
export r2s_root="$HOME/github/dev/kit-at"

# Path to the activation driver executable
export r2s_driver=$r2s_root/adriver/adriver.exe

# Path to the scripts that are called by $r2s_driver. THese are  initialization
# and finalization scripts that are usually cluster-dependent, but
# case-independent, and scripts that prepare fispact working folders and run
# fispact.
export r2s_scripts=$r2s_root/scripts

export R2s_init_1=$r2s_scripts/init_1.sh
export R2s_init_n=$r2s_scripts/init_n.sh
export R2s_finalize_1=$r2s_scripts/finalize_1.sh
export R2s_finalize_n=$r2s_scripts/finalize_n.sh

# Path to the default natural abundancies (used only when ZZ000 nuclides are found in $R2s_matcomposition)
# Used by the driver
export R2s_natab=$r2s_root/files/natural.txt


# Path to the fispact data and executable.
export r2S_fispact_data=/marconi_work/FUA11_MCHIFI/atr_share/fispact-II-r2s
export r2S_fispact_exe=`which fispact-II`

# Fispact input files for condense, collapse and inventory calculations. These files are uUsually case-independent.
export r2S_condense_input=$r2s_root/files/condense_input
export r2S_condense_files=$r2s_root/files/condense_files
export r2S_collapse_input=$r2s_root/files/collapse_input
export r2S_collapse_files=$r2s_root/files/collapse_files
export r2S_inventory_input_header=$r2s_root/files/inventory_input_header
export r2S_inventory_files=$r2s_root/files/inventory_files

# Scripts to run condense, collapse and inventory. 
# Scripts $*_s1 prepare fispact workplace and run it. Scripts $*_s2 clean up the workspace (if needed)
# Used by the driver
export R2s_condense_s1=$r2s_scripts/condense_s1.sh
export R2s_condense_s2=$r2s_scripts/condense_s2.sh
export R2s_collapse_s1=$r2s_scripts/collapse_s1.sh
export R2s_collapse_s2=$r2s_scripts/collapse_s2.sh
export R2s_inventory_s1=$r2s_scripts/inventory_s1.sh
export R2s_inventory_s2=$r2s_scripts/inventory_s2.sh


# Folder with the case input data
export r2s_input=$(realpath ./input)

# Folder where gamma intensities are written by each process.
# This must be a global file system, to ensure files remain for
# restart
export R2s_out=$(realpath ./out)

# Folder where each process writes its log file
export R2s_log=$(realpath ./log)



# Case input files 
export R2s_matallocation=$r2s_input/fine_mesh_content
export R2s_neutronintensity=$r2s_input/meshtal.fine
export R2s_neutronspectra=$r2s_input/meshtal.coarse
export R2s_matcomposition=$r2s_input/mat_table
export R2s_cellsmaterials=$r2s_input/cmi_table
export r2S_inventory_input_footer=$r2s_input/inventory_input_footer


# Scratch folder. If node-specific, ensure that these folders are created in $r2s_scripts/init_n.sh script
# Scratch files are written to R2s_scratch/r2s_w and R2s_scratch/r2s_r
export R2s_scratch='/scratch_local'
