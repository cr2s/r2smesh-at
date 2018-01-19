#!/bin/bash

# compile adriver.exe

. ./modules.sh

rm *.mod *.o adriver.exe

$fcc $opt \
            -o adriver.exe \
            proc_module.f90 \
            gen_module.f90 \
            meshtal_module.f90 \
            matcomp_module.f90 \
            matall_module.f90 \
            env_module.f90 \
            fd_module.f90 \
            adriver.f90 \
