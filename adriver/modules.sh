#!/bin/bash

# Load machine-dependent modules for fortran + mpi 

host=$(hostname -s)
case $host in
    "local"*)
        # local laptop
        fcc=$(which mpif90)
        opt="-fbacktrace"
        ;; 
    "fh2"*)
        # forHLR-II
        module load compiler/intel/17.0 mpi/impi/2017
        fcc=$(which mpiifort)
        opt="-mcmodel=large -debug full -traceback -gen-interfaces -warn interfaces -check -fpe0" 
        ;;
    "r000u"*)
        # Marconi cluster
        module load intel intelmpi
        fcc=$(which mpiifort)
        opt="-mcmodel=large -debug full -traceback -gen-interfaces -warn interfaces -check -fpe0" 
        ;;
    *)
        # Unknown machine
        echo "No module loaded"
        ;;
esac;         

