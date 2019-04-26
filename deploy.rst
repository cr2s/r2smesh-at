Prerequisites
======================
The r2smesh system provided in this repository organizes the data flow from MCNP5 neutron transport calculations to the inventory calculations with FISPACT-II and further to the MCNP5 gamma transport calculations. MCNP and FISPACT neither distributed here nor required during at compile time of the r2smesh code. The user shall obtain MCNP and FISPACT separately. However, some modifications to the MCNP code are necessary to provide all data for the inventory calculations, and to properly sample the decay gamma source. The MCNP code modifications are provided in this repository as the patch files. Therefore, the use of the r2smesh system assumes that the user has the source code of MCNP. 

The central part of r2smesh is the activation driver -- a fortran program that prepares FISPACT-II input files based on the neutron flux and material distribution, and starts different steps of the inventory calculations in parallel via MPI. The activation driver requires Intel Fortran and Intel MPI implementation. Several shell scripts for pre- and postprocessing were written for and tested with the Bash shell. 

The r2smesh system was developed and tested only under the linux OS.



INSTALLATION INSTRUCTIONS
================================

1. Get the source code of r2smesh, for example by cloning this repository to your account::

      >cd ~/work
      >git clone git@github.com:cr2s/r2smesh-at.git
      
   These commands put the content of this repository to the ``work/r2smesh-at`` folder in your account. 


2. Compile the activation driver. Currently, the Intel fortran together with
   the Intel MPI implementation is required. The `<adriver/compile.sh>`_ script
   contains presets for compilation instructions for several machines, among
   them the Marconi cluster and a local linux machine with Intel Fortran and
   intel-MPI installed to their default places::

      >cd r2smesh-at/adriver
      >./compile.sh   

   If compilation successful, the executable ``adriver.exe`` appears.


3. Set environment variables (optional). 
   
   It suffices when the ``fispact-II`` executable is found in ``$PATH``. 
   Otherwise, its location can be set explisitly with the ``$FISPACT`` environment  variable. 
   Location of the decay data used by FISPACT-II for activation
   calculations can be set with the ``$FISPACT_DATA`` environment variable. If
   this variable is undefined, the default data path, guessed from the location
   of ``fispact-II`` executable, is applied.

   The activation driver communicates to FISPACT-II via input/output files. The
   user can specify the place where the input and output files are written via
   the ``$R2S_SCRATCH`` environmental variable. By default, a subfolder
   ``scratch`` is created within the problem workspace. This is suitable for
   debugging and small problems; for massively parallel runs of the activation
   driver consider using the node's local filesystem.



4. Test the activation driver

   The `<example>`_ folder contains an example set of the files necessary to run the
   activation driver, i.e. it is assumed that the neutron transport and material
   detection steps are already completed and their results are processed properly
   to get all necessary input files. Details how the input files are prepared see in `<docs>`_. 

   By default, all problem-specific files necessary to perform activation
   calculations are placed in the ``input`` subfolder. It contains neutron flux
   intensity and spectra, material distribution, irradiation scenario. The
   activation driver during its execution creates additionally subfolders
   ``scratch`` for temporary files, ``log`` for log files and ``out`` for the
   results. 

   Location of all files required by the activation driver (their path and
   names) are defined via environment variables. The `<scripts/r2s_env.sh>`_
   initialization script, sourced from the activation driver working directory
   provides their default values. 

   To start activation calculations in ``example``, go there and source the
   initialization script::

      >cd r2smesh-at/example
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
   must be processed with the `<scripts/form_dgs.sh>`_ script, which takes the name of the folder where ``cgi.*`` files are located and 
   a list of irradiation step numbers, for whose the decay gamma source files must be prepared. ::

      > scripts/form_dgs.sh out 45 46 47

   the files ``out/dgs.45``, ``out/dgs.46`` and ``out/dgs.47`` represent the
   decay gamma sources at 45-th, 46-th and 47-th irradiation steps and can be
   used in the modified MCNP5 to model the decay gamma transport. 

 
   
5. Apply MCNP patches 

   The files describing material spatial distribution and isotopic compositions (these are the input files for the activation driver; their default names are ``input/fine_mesh_content`` and ``input/mat_table``) can be prepared with the version of MCNP5 modified with `<mcnp-mod/kit-materials.patch.5>`_. 
   
   The ``dgs.NN`` files contain spatial and spectral distribution of the decay gamma source. They can be read directly to MCNP5 with the custom source subroutine provided in the `<mcnp-mod/kit-gamma.patch.5>`_ patch. 
   
   TODO: describe how to apply the patches.

   

