#!/bin/bash

# Compine outcells* to a single fine_mesh_content
cat outcells? outcells?? > _1.txt
sort -g -k1 -k2 -k3 _1.txt > fine_mesh_content
