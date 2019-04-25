GENERAL INFO
======================

This distribution comprises programs and scripts to organize dataflow
from MCNP neutron transport calculations to FISPACT and from FISPACT activation
calculations to the MCNP decay gamma transport. 

FISPACT activation calculations require as input data the neutron flux spatial
and energy distribution and the distribution of material to be activated. The
neutron flux distribution must be calculated by two overlapping mesh tallies (one
for the flux intensity, the other for flux spectra) of the standard MCNP5
(currently only rectangular meshes are supported). To get the material
distribution, the version of MCNP5 modified with the patch
``mcnp-mod/kit-materials.patch.5`` can be used. FISPACT calculations for every
neutron flux intensity, spectrum and the correpondent materials are organized
by ``adriver/adriver.exe``. The results of FISPACT calculations are processed
to generate the decay gamma source file.  The latter can be used in the version
of MCNP5 modified with the path ``mcnp-mod/kit-gamma.patch.5`` to model decay
gamma transport.

This repository contains the both MCNP5 patches and all files necessary to run the
activation driver. 

INSTALLATION INSTRUCTIONS
================================

1. Copy or clone the repository::

      >git clone git@github.com:cr2s/r2smesh-at.git


2. Compile the activation driver. Currently, the Intel fortran together with
   the Intel MPI implementation is required. The ``adriver/compile.sh`` script
   contains presets for compilation instructions for several machines, among
   them the Marconi cluster and a local linux machine with Intel Fortran and
   intel-MPI installed to their default places::

      >cd r2smesh-at/adriver
      >./compile.sh   

   If compilation was successful, the executable ``adriver.exe`` appears.


3. Set environment variables (optional). 
   
   FISPACT-II is used by the activation driver to perform activation
   calculations. The ``fispact-II`` executable must be found in ``$PATH``, or
   its location can be set explisitly with the ``$FISPACT`` environment
   variable. Location of the decay data used by FISPACT-II for activation
   calculations can be set with the ``$FISPACT_DATA`` environment variable. If
   this variable is undefined, a default data path, guessed from the location
   of ``fispact-II`` executable, is applied.

   The activation driver communicates to FISPACT-II via input/output files. The
   user can specify the place where the input and output files are written with
   the ``$R2S_SCRATCH`` environmental variable. By default, a subfolder
   ``scratch`` is created within the problem workspace. This is suitable for
   debugging and small problems; for massively parallel runs of the activation
   driver consider using the node's local filesystem.



4. Test the activation driver

   The ``example`` folder contains an example set of the files necessary to run the
   activation driver, i.e. it is assumed that the neutron transport and material
   detection steps are already completed and their results are processed properly
   to get all necessary input files. Details how the input files are prepared see in ``docs``. 

   By default, all problem-specific files necessary to perform activation
   calculations are placed in the ``input`` subfolder. It contains neutron flux
   intensity and spectra, material distribution, irradiation scenario. The
   activation driver during its execution creates additionally subfolders
   ``scratch`` for temporary files, ``log`` for log files and ``out`` for the
   results. 

   Location of all files required by the activation driver (their path and
   names) are defined via environment variables. The ``scripts/r2s_env.sh``
   initialization script, sourced from the activation driver working directory
   provides their default values. 

   To start activation calculations in ``example``, go there and source the
   initialization script::

      >cd example
      >source ../scripts/r2s_env.sh

   
   The last command prints out the list of variables that will be passed to the
   activation driver. Now, the activation driver can be started::

      >$r2s_driver

   Alternatively, it can be started in parallel (here with 4 threads)::

      >mpiexec.hydra -n 4 $r2s_driver

   Debug information from all threads of the activation driver is written to
   ``log/log.N`` files, where ``N`` is the thread's number. A successful
   completion of the activation calculations is denoted with the line
   ``========== 2019/04/26 00:10:31 Program completed`` in ``log.0``. 

   When the activation driver completes, the ``out`` folder contains several
   ``cgi.*.*.*`` files, containing decay gamma source information (intensities
   and spectra) for fine mesh elements at each irradiation step. One ``cgi.*`` file corresponds to one
   coarse mesh element. The ``cgi.*`` files can have different number of lines
   (different sizes) since only fine mesh elements with non-zero decay gamma
   intensity are mentioned. 

   To prepare a decay gamma source for particular irradiation step, the separate ``cgi.*`` files 
   must be processed with the ``scripts/form_dgs.sh`` script, which takes the name of the folder where ``cgi.*`` files are located and 
   a list of irradiation step numbers, for whose the decay gamma source files must be prepared. ::

      > scripts/form_dgs.sh out 45 46 47

   the files ``out/dgs.45``, ``out/dgs.46`` and ``out/dgs.47`` represent the
   decay gamma sources at 45-th, 46-th and 47-th irradiation steps and can be
   used in the modified MCNP5 to model the decay gamma transport. 

 
   
5. Apply MCNP patches 

   to be written.    


