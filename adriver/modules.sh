#!/bin/bash

# Load machine-dependent modules for fortran + mpi 

host=$(hostname -s)
case $host in
    "fh2"*)
        # forHLR-II
        module load compiler/intel/17.0 mpi/impi/2017
        ;;
    "r000u"*)
        # Marconi cluster
        module load intel intelmpi
        ;;
    *)
        # Unknown machine
        echo "No module loaded"
        ;;
esac;         

