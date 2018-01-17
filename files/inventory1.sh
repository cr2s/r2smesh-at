#!/bin/bash

# Script creates folder $1 for inventory fispact run.
# The collapsed xs are taken from the $2 folder.
# Input file is constructed by the driver.


wd="$r2s_fwd/inv/$1/$2/$3.$4" 
cl=$(realpath "$r2s_fwd/col/$5/$6/$7")
cn=$(realpath "$r2s_fwd/cond")

if [ ! -d "$wd" ]; then
    o="$(pwd)"

    mkdir -p "$wd"
    ln -s "$o"/inv_files.template "$wd"/files
    ln -s "$o"/inv_input.header "$wd"/inv_input.header
    ln -s "$cl"/collapx "$wd"/collapx
    ln -s "$cn"/arrayx "$wd"/arrayx
    ln -s "$r2s_fsp_data" "$wd"/fispact-data

    # Currently, the input template is provided as a single file (for convenience).
    # It is split in the fispact inventory folder into the header and footer. The
    # header is copied to the input as is by the next script, material is added by
    # the driver and the footer (irradiation scenario), adjusted by the driver, is
    # copied to the input by the next script.
    mv "$r2s_scratch/scenario.$1.$2.$3.$4"     "$wd"/.
    mv "$r2s_scratch/mat.content.$1.$2.$3.$4"  "$wd"/.
    mv "$r2s_scratch/mat.title.$1.$2.$3.$4"    "$wd"/.
    cd $wd
    if [ ! -f inventory.tab4 ]; then
        # Currently, the input file name `inventory` is hardcoded in fortran!
        cat ./inv_input.header \
            ./mat.title.$1.$2.$3.$4 \
            ./mat.content.$1.$2.$3.$4 \
            ./scenario.$1.$2.$3.$4 > inventory.i
        "$r2s_fsp_exe" inventory > fispact.out
        cp inventory.tab4 $(realpath $r2s_scratch)/tab4.$1.$2.$3.$4
    fi    
    cd ..
    exit 0
fi  
exit 1
