#!/bin/bash

# Compine outcells* to a single fine_mesh_content
if [ -f outcells1 ]; then
    echo "merging outcells? ..."
    cat outcells? > _1.txt
fi
if [ -f outcells10 ]; then
    echo "merging outcells?? ..."
    cat outcells?? >> _1.txt
fi
if [ -f outcells100 ]; then
    echo "merging outcells??? ..."
    cat outcells??? >> _1.txt
fi
if [ -f outcells1000 ]; then
    echo "merging outcells???? ..."
    cat outcells???? >> _1.txt
fi
echo "Sorting merged file ..."
sort -g -k1 -k2 -k3 _1.txt > fine_mesh_content
