#!/bin/bash

# target file for cat should be on another disk
scratch=$CINECA_SCRATCH
ctgt="$scratch/__dgs_all.1"
stgt="$scratch/__dgs_all.2"
trst="$scratch/__dgs_all.r"



echo "Concatenate gi" files to $ctgt
cat "$r2s_fwd"/gi/gi.* > $ctgt 
echo "Sort concatenated file by time intervals"
sort -gb -k1 $ctgt > $stgt 

echo "Split into time intervals"
cp $stgt $trst.1

# Split __dgs_all.2 untill error is found
dgsf="$r2s_fwd"/dgs
mkdir -p "$dgsf"
i=1
contloop=true
while $contloop; do
    j=$(expr $i + 1)
    echo "     Extracting ..."
    csplit -s $trst.$i "/^  *$j /"
    e=$?   # store exit status of csplit
    if [ 0 == "$e" ]; then
        # csplit exit status is 0, i.e. no errors, i.e. the pattern was found
        mv xx00 dgs.$i.0
        mv xx01 $trst.$j
        rm -f xx??
    else
        cp $trst.$i dgs.$i.0
        contloop=false
    fi

    # Sort in the i, j, k order
    echo "     sorting ..."
    sort -gb -k2 -k3 -k4 dgs.$i.0 > dgs.$i.1
    # Add mesh description and number of fine mesh elements
    cp fine_mesh_def "$dgsf/dgs.$i"
    wc -l < dgs.$i.1 >> "$dgsf/dgs.$i"
    cat dgs.$i.1 >> "$dgsf/dgs.$i"
    echo "     DGS for time interval $i written to:   $dgsf/dgs.$i"
    rm dgs.$i.[01]

    # Prepare for the next cycle
    i=$j
done
