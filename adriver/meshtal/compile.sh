#!/bin/bash

# Compile test together with the module
. ../modules.sh

rm *.o *.mod *.exe


ifort -debug full -o test.exe \
       gen_module.f90 \
       meshtal_module.f90 \
       test.f90
