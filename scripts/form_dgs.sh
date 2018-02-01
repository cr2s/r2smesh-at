#!/bin/bash

# Form dgs from cgi in folder $1 for time interval $@
f=$1
shift 1
echo "Forming DGS from files in $f for time intervals $@"
date
echo

for i in $(seq 1 9); do
    # ni -- number of files to process
    ni=$(\ls -1 $f/cgi.${i}* 2>/dev/null | wc -l)
    if [ $ni -gt 0 ]; then
        echo "    Extracting lines from cgi.$i  ($ni files)"
        for n in "$@"; do
            N=$(printf "%6g" $n)
            echo -n "        for time interval $n ... "
            grep -h "^$N " $f/cgi.${i}* > /scratch_local/dgs.$n.$i 
            date
        done
    fi
done

for n in "$@"; do
    echo -n "Forming DGS file for time interval $n ... "
    # Concatenate parts to a single file and add the dgs header
    cp $f/dgs.header                                    $f/dgs.$n
    echo $(cat /scratch_local/dgs.$n.[1-9] | wc -l)  >> $f/dgs.$n
    sort -bg -k2 -k3 -k4 /scratch_local/dgs.$n.[1-9] >> $f/dgs.$n
    echo " written to $f/dgs.$n $(date)"
done    

exit