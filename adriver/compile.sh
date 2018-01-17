#!/bin/bash

# compile adriver.exe 

# For for-HLR-II
module load compiler/intel/17.0 mpi/impi/2017

mpiifort -mcmodel=large -debug full \
            -o adriver.exe \
            io_module.f90 \
            env_module.f90 \
            gen_module.f90 \
            meshtal_module.f90 \
            fd_module.f90 \
            adriver.f90 \
