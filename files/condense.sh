#!/bin/bash

# Script creates folder for condense fispact run.

d="$r2s_fwd/cond"
mkdir -p "$d"
cp cnd_files.template "$d"/files
cp cnd_input.template "$d"/condense.i
ln -s "$r2s_fsp_data" "$d"/fispact-data

cd "$d"
"$r2s_fsp_exe" condense > fispact.out
cd ..

