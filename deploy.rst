GENERAL INFO
======================

This distribution comprises files (programs and scripts) to organize dataflow
from MCNP neutron transport calculations to FISPACT and from FISPACT activation
calculations to the MCNP gamma transport. 

FISPACT activation calculations require as input data the neutron flux spatial
and energy distribution and the distribution of material to be activated. The
neutron flux distribution must be calculated by overlapping mesh tallies (one
for the flux intensity, the other for flux spectra) of the standard MCNP5
(currently only rectangular meshes are supported). To get the material
distribution, the version of MCNP5 modified with the patch
``mcnp-mod/kit-materials.patch.5`` can be used. FISPACT calculations for every
neutron flux intensity, spectrum and the correpondent materials are organized
by ``adriver/adriver.exe``. The results of FISPACT calculations are processed
to generate the decay gamma source file.  The latter can be used in the version
of MCNP5 modified with the path ``mcnp-mod/kit-gamma.patch.5`` to model decay
gamma transport.


Apply  MCNP5 patches
======================

to be written

INSTALL FISPACT driver
============================
The activation driver ``adriver/adriver.exe`` reads the neutron flux mesh tallies,
material distribution and composition and the irradiation scenario provided by
the user, and prepares parts of the input files for FISPACT. The acrivation driver does
not start FISPACT directly, instead, the preparation of the directories for
FISPACT runs as well as starting FISPACT are described by shell scripts in
``scripts``, which are started by the activation driver (creation of folders,
concatenation of parts of the FISPACT input files etc. is better described by
shell scripts as with FORTRAN).



1. Copy or clone the repository::

      >git clone git@github.com:cr2s/r2smesh-at.git


2. Compile the activation driver. Currently, the Intel fortran together with the
   Intel MPI implementation is required::

      >cd r2smesh-at/adriver
      >./compile.sh   

   If compilation was successful, the executable ``adriver.exe`` appears.


3. Set environment variables (optional). 
   
   FISPACT-II is used by the activation driver to perform activation calculations. The
   ``fispact-II`` executable must be found in ``$PATH``, or its location can be
   set explisitly with the ``$FISPACT`` environment variable. Location of the
   decay data used by FISPACT-II for activation calculations can be set with
   the ``$FISPACT_DATA`` environment variable. 

   The activation driver communicates to FISPACT-II via input/output files. The user can
   specify the place where the input and output files are written with the
   ``$R2S_SCRATCH`` environmental variable.

   The above variables are optional: if not set, they are guessed. 


4. Test the activation driver

   The ``example`` folder contains an example set of the files necessary to run the
   activation driver, i.e. it is assumed that the neutron transport and material
   detection steps are already completed and their results are processed properly
   to get all necessary input files. Details how the input files are prepared see
   below in ``Input files for the activation driver``. 

   By default, the activation driver working place has the ``input`` subfolder
   containing input files specific to the problem (neutron flux intensity and
   spectra, material distribution, irradiation scenario). The activation driver
   during its execution creates additionally subfolders ``scratch`` for temporary files,
   ``log`` for log files and ``out`` for the results. 

   Location of all files required by the activation driver (their path and names) are 
   defined via environment variables. The ``scripts/r2s_env.sh`` initialization script, sourced from the
   activation driver working directory provides their default values. 

   To start activation calculations in ``example``, go there and source the
   initialization script::

      >cd example
      >source ../scripts/r2s_env.sh
   
   The last command prints out the list of variables that will be passed to the activation 
   driver. Now, the activation driver can be started::

      >$r2s_driver

   It can be started also in parallel (here with 4 threads)::

      >mpiexec.hydra -n 4 $r2s_driver


