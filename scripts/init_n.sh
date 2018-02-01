#!/bin/sh

# This script is executed once on each node. 

# It should be started with the node identifier (node name, node number,
# process number etc.) passed as the 1-st command line argument.

n=$1.$2

# Recreate scratch folders, where each process will make IO
rm -rf $r2s_scratch/r2s_w   > init_n.$n.log
rm -rf $r2s_scratch/r2s_r  >> init_n.$n.log
rm -rf $r2s_scratch/r2s_c  >> init_n.$n.log
rm -rf $r2s_scratch/r2s_i  >> init_n.$n.log

mkdir -vp $r2s_scratch/r2s_w >> init_n.$n.log
mkdir -vp $r2s_scratch/r2s_r >> init_n.$n.log
mkdir -vp $r2s_scratch/r2s_c >> init_n.$n.log
mkdir -vp $r2s_scratch/r2s_i >> init_n.$n.log

# Copy some often-used files to the nodes
cp -v "$(realpath ./cond/arrayx)"   "$r2s_scratch"/r2s_r/arrayx          >> init_n.$n.log

cp -rv "$r2S_fispact_data"          "$r2s_scratch"/r2s_r/fispact-data    >> init_n.$n.log

cp -v "$r2S_collapse_files"         "$r2s_scratch"/r2s_r/collapse_files  >> init_n.$n.log
cp -v "$r2S_collapse_input"         "$r2s_scratch"/r2s_r/collapse_input  >> init_n.$n.log

cp -v "$r2S_inventory_files"         "$r2s_scratch"/r2s_r/inventory_files  >> init_n.$n.log
cp -v "$r2S_inventory_input_header"  "$r2s_scratch"/r2s_r/header           >> init_n.$n.log
cp -v "$r2S_inventory_input_footer" "$r2s_scratch"/r2s_r/footer            >> init_n.$n.log

exit 0

