#!/bin/bash

# Check that the r2s_scratch is defined. If not, use system default
if [ ! -v r2s_scratch ]; then
    echo "Default value for r2s_scratch will be used"
    . `dirname $0`/r2s_env.sh > /dev/null
fi

# Form dgs from cgi in folder $1 for time interval $@
out=$1
shift 1
echo "Forming DGS from files in $out for time intervals $@"
date
echo

tmp=$(mktemp -p $r2s_scratch -t r2s.dgs.XXXX -d)
echo "Temporary files written to $tmp"

for i in $(seq 1 9); do
    # ni -- number of files to process
    ni=$(\ls -1 $out/cgi.${i}* 2>/dev/null | wc -l)
    if [ $ni -gt 0 ]; then
        echo "    Extracting lines from cgi.$i  ($ni files)"
        for n in "$@"; do
            N=$(printf "%6g" $n)
            echo -n "        for time interval $n ... "
            grep -h "^$N " $out/cgi.${i}* > $tmp/dgs.$n.$i 
            date
        done
    fi
done

for n in "$@"; do
    echo -n "Forming DGS file for time interval $n ... "
    # Concatenate parts to a single file and add the dgs header
    cp $out/dgs.header                                    $out/dgs.$n
    echo $(cat $tmp/dgs.$n.[1-9] | wc -l)  >> $out/dgs.$n
    sort -bg -k2 -k3 -k4 $tmp/dgs.$n.[1-9] >> $out/dgs.$n
    echo " written to $out/dgs.$n $(date)"
done    

rm -rf $tmp

exit
