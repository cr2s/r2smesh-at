#!/bin/bash

# tovtk type=dgsN assumes that the fine_mesh_content file has the header, which is gegerated by the driver.
# This scripts adds the header and the number of lines in the ofirinal fine_mesh_content.

i=$1  # original fine_mesh_content
h=$2  # header
o=$1.dgsN

if [ -f "$o" ]; then 
    echo "Output file $o exists."
    exit 1
else
    echo "Original fine_mesh_content file: $i"
    echo "File containing header: $h"
fi



cp $h $o
echo $(cat "$i" | wc -l) >> $o
cat $i >> $o

