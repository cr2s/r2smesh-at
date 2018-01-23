#!/bin/bash

# Script creates folder $1 for inventory fispact run.
# The collapsed xs are taken from the $2 folder.
# Input file is constructed by the driver.

wdr="$r2s_fwd"/inv/$1/$2/$3   # fispact working folder
cld="$r2s_fwd"/col/$4/$5/$6   # collapse working folder
cnd="$r2s_fwd"/cond           # condense working folder
sdr="$r2s_scratch"            # scratch folder
cdr="$(pwd)"                  # current folder (where adriver starts)


# Do something only when corresponding tab4 does not exist
if [ -a "$sdr"/tab4.$1.$2.$3 ]; then 
   exit 0
fi   


# Prepare working directory, if not exists
if [ ! -d "$wdr" ]; then

    mkdir -p "$wdr"
    # LInks to files and fispact data folder
    ln -s "$cdr"/inv_files.template "$wdr"/files
    ln -s "$r2s_fsp_data" "$wdr"/fispact-data

    # Links to condense and collapse results
    ln -s "$cld"/collapx "$wdr"/collapx
    ln -s "$cnd"/arrayx "$wdr"/arrayx

    # Currently, the input template is provided as a single file (for convenience).
    # It is split in the fispact inventory folder into the header and footer. The
    # header is copied to the input as is by the next script, material is added by
    # the driver and the footer (irradiation scenario), adjusted by the driver, is
    # copied to the input by the next script.

    # Concatenate parts of the input file
    cat "$cdr"/inv_input.header     \
        "$sdr"/mat.title.$1.$2.$3 \
        "$sdr"/mat.content.$1.$2.$3 \
        "$sdr"/scenario.$1.$2.$3 > "$wdr"/inventory.i
fi

# Execute fispact if inventory.tab4 not exists
if [ ! -f "$wdr"/inventory.tab4 ]; then
    cd "$wdr"

    # Execute fispact and exit if fispact exit status was non-zero
    "$r2s_fsp_exe" inventory > fispact.out  || exit 1
    cd "$cdr"
fi    
mv "$wdr"/inventory.tab4 "$sdr"/tab4.$1.$2.$3

exit 0
