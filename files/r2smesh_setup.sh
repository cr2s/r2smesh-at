#!/bin/bash

# Call this script on a single process

. r2smesh_env.sh

mkdir -p "$r2s_fwd"/col
mkdir -p "$r2s_fwd"/inv
mkdir -p "$r2s_fwd"/gi

mkdir -p "$r2s_scratch"

# SPlit the inventory input into the header and footer
csplit -s inv_input.template '/<< Irradiation Scenario/' 
mv xx00 inv_input.header
mv xx01 inv_input.footer


