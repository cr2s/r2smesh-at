#!/bin/bash

#MSUB -l nodes=4:ppn=20
#MSUB -l walltime=1:00:00
#MSUB -N test

# These modules are used for compilation 
module load compiler/intel/17.0 mpi/impi/2017

. r2s_env.sh
export r2s_continue="no"

mpiexec.hydra -bootstrap pbsdsh $r2s_driver

