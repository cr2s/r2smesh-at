#!/bin/bash

# Script around the MCNP' make utility. It should be placed to the root folder
# containing `Makefile`, usually this is the "Source" folder.

# All command line arguments provided to this script are passed to ``make
# build``.  This is to provide possibility to run compilation in parallel, e.g.
# with "-j 4" option.

# Backup existing ../bin/mcnp?.mpi executables! The `make build` called from
# this script rewrites the executable `../bin/mcnp?.mpi`. The script removes
# all files matching  this pattern before calling the make utility.

# Modules to load and variables to pass to the make utility depend on
# particular cluster. Here the cluster name is defined to choose later proper
# modules and variables
host=$(hostname -s)

# Compilation messages redirected here.
log=compile.log

# Information about applied patches is added by the patch_apply.sh
# script to patches.log file. Its content is passed here to the
# DATAPATH variable and thus -- to the mcnp executable.
if [[ -f ../patches.log ]]; then
    DATAPATH=$(echo $(cat ../patches.log))
    # Replace spaces with colon:
    shopt -s extglob  # needs to be turned on for +() extended pattern below
    DATAPATH=${DATAPATH//+( )/":"}
    shopt -u extglob
    export DATAPATH
fi    
echo "Information about applied patches is written to \$DATAPATH:"
echo "The following will appear in mcnp executable: $DATAPATH"

# Default value. It is changed below for known clusters
export CONFIG="mpi omp  plot intel"

# Identify cluster and set modules and variables
case $host in
    "ic2"*)
        # ic2.scc.kit.edu, cluster at KIT
        module load intel/15.0.2  impi/5.0.3
        export MPIFC=mpif90
        export MPICC=mpicc
        export OMPI_CFLAGS=""
        export FOPT="-shared-intel"
        ;;
    "helios"*)
        # helios.iferc-csc.org
        module load intel intelmpi
        export FOPT="-shared-intel"
        ;;
    "uc1"*)
        # uc1.scc.kit.edu, cluster at KIT
        module load compiler/intel/15.0  mpi/impi/5.0.3-intel-15.0
        export MPI_ROOT=$MPI_HOME
        export MPIFC=mpif90
        export MPICC=mpicc
        export FOPT="-shared-intel"
        # I use uc1 for anps5. Use here DATAPATH for usual purposes:
        export DATAPATH="/home/kit/inr/rx8040/anps5/data"
        ;;
    "r000u"*)
        # the marconi cluster.
        module load intel intelmpi
        export FOPT="-shared-intel"
        ;;
    "fh2"*)
        # ForHLR-II cluster
        module load compiler/intel/17.0 mpi/impi/2017
        export FOPT="-shared-intel" 
        ;;
    *)
        # unknown cluster. 
        export CONFIG=""
        ;;
esac;


if [[ -n $CONFIG ]]; then
    # remove previous executable
    rm -f -v ../bin/mcnp?.mpi  # -f for ingnoring nonexisting files

    # Store current module configuration to log
    module list 2>&1 | tee $log
    # run make
    make  build "$@"  2>&1 | tee -a $log

    # If mcnp?.mpi created, rename it and put log file 
    cd ../bin
    exe=$(ls mcnp?.mpi)  # here should be a single file, since we removed all such files previously
    if [[ -n "$exe" ]]; then
        # Time stamp is used to identify generated executables and corresponding log files.
        dstamp=$(date +%Y%m%d__%H_%M_%S)
        mv -v $exe $dstamp.$exe
        ln -s -v $dstamp.$exe $exe
        mv -v ../Source/$log $dstamp.log
    fi;
fi;    


