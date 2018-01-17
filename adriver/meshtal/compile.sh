#!/bin/bash

# Compile test together with the module

rm *.o *.mod *.exe

# For forHLR-II
module load compiler/intel/17.0

ifort -debug full -o test.exe \
       gen_module.f90 \
       meshtal_module.f90 \
       test.f90
