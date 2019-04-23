#!/bin/bash

# Load machine-dependent modules for fortran + mpi 

# TODO: prepare two sets of opts: for debug (with traceback etc) and for production.

host=$(hostname -s)
case $host in
    "local"*)
        # local laptop
        source /opt/intel/bin/compilervars.sh intel64
        fcc=$(which mpiifort)
        opt="-mcmodel=large -debug full -traceback -gen-interfaces -warn interfaces -check -fpe0"
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

