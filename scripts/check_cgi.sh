#!/bin/bash

# Check cgi files. Only those containing `_` on the last line
# are intact
for f in cgi.*; do
    tail -n 2 $f |  grep -q "_" || echo $f
done


