#!/bin/bash

meshtal=${1:-'meshtal'}
if [ ! -f "$meshtal" ];  then
    echo "Specify meshtal file to split"
    exit 1
else
    echo "Splitting file $meshtal"
fi

echo "Splitting meshtal into chunks:"
rm xx??
csplit  $meshtal '/Mesh Tally /' '{*}'

echo "Renaming chunks:"
cmnd=""
i=1
for n in $(grep "Mesh Tally" $1| awk '{print $NF}'); do
    chunk=xx`printf "%02d" $i`
    mv -v $chunk $meshtal.tall.$n
    let i++
    # cmnd="$cmnd "'/Mesh Tally .*'"$n"'/'
    # echo "$cmnd"
done
