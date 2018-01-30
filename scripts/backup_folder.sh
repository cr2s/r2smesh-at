#!/bin/sh

# Backup folder specified as the 1-st command line argument
folder="$1"
if [ -d $folder ]; then
    # the folder allready exists. Move it to *.old.N, where N from 1 to ...
    i=0
    while true; do
        i=$(expr $i + 1)
        if [ ! -d $folder.old.$i ]; then 
            mv -v $folder $folder.old.$i
            exit 0
        fi
    done
fi
exit 0
