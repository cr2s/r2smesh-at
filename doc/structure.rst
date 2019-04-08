Purpose
===========
`r2smesh-at` is an implementation of the rigorous 2-step approach to predict
photon field (and derived values like shut-down dose rate, SDDR) due to photons
emmited by neutron-activated material. In this approach, neutron transport to
predict neutron flux intensities and spectra for material activation, and the
following decay photon transport are done in two separate calcualtions (thus 2
steps). Neutron transport is computed in a mesh superimposed onto the model geometry.
Activation analysis is done in each element of the mesh using the neutron flux
intensity and spectra computed in that mesh element and for materials found in
that mesh element. Activation analysis gives decay photon intensity and spectra
in each mesh element. This information is used to sample sources for the photon
transport. Thus, distribution of neutron flux, material and decay photons is
described using the superimposed mesh, as opposite to another approach, where
average neutron flux is computed for particular material and average value of
gamma intensity is used for sampling decay photons.

MCNP5 is used for neutron and photon transport. Fispact-II is used for material
activation.  The package helps to organize data flow from neutron transport
simulation to material activation, and from material activation to photon
transport. The central part of the implementation is the driver running
activation calculations for each mesh element. This driver reads neutron flux
distribution, material distribution, prepares Fispact input files for each mesh
element and runs Fispact. Meshes for real applications have about 10^7
elements, which implies high requirements to the effeciency of the driver.
Essential is parallel invocation of Fispact and effective use of file system
(Fispact is available as executable, all communication is via external files).

Netron flux distribution in a mesh is obtained with MCNP5. Material
distribution in the mesh and material nuclide compositions are extracted from
the MCNP model with a modified MCNP5 version. Neutron flux distribution,
material distribution and compositions are supplied to the fispact driver,
which generates fispact input files, runs fispact, reads fispact's output to
extract information about decay photons for each mesh element.  Finally, a
modified version of MCNP5 is used for photon transport. This modified version
has custom source sampling subroutine that reads decay photon distribution from
the file prepared by the fispact driver.



Pacakge content overview
==========================

The package has two different patches to MCNP5, necessary to compile modified
versions for obtaining mateiral distribution and for decay gamma sampling. The
user must have MCNP5 source code to apply patches.

The package contains source code for the fispact driver. All file operations
necessary to create fispact work places (folders containing all necessary input
files) are not hardcoded in the driver. Instead, it calls various shell
scripts. The shell script templates are provided with the package as well and
can be modified by the user without necessity to recompile the driver thus
adjustment for particular computer. 

Details of the activation analysis (computational options, irradiation
scenario) must be supplied by the user as fispact input file templates. The
package contains standard input files that can be applied unchanged or provide
basis for case-specific files.

Finally, the package contains several shell scripts that simplify intermediate
manipulation of files.


MCNP patches
--------------
TODO: how pathes applied. 

MCNP patch for material detection
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

MCNP patch for material detection is `kit-materials.patch.5`_.
This patch makes MCNP5 to sample homogeneously coordinates in each mesh element
and count material hits. This is done only by the MPI slave processes; each MPI
process writes the number of hits into file ``outcellsNN``, where ``NN`` is the
process index (from 1 to N-1, where N is the total number of MPI processes MCNP
is started with). Since coordinate sampling in the mesh element is homogeneous,
the number of material hits is proportional to the volumetric fraction of that
material in the mesh element.  The more coordinate points sampled the better
statistical presicion of the obtained volumetric fractions. The number of
sampled coordinates is controlled by the 1-st entry on the ``IDUM`` card, which
defines the number of samped coordinates per 1 cm3. By default it is set to 100
(or 1000?).

The second entry on the ``IDUM`` card specifies the meshtally name, where
materials are detected.  If not specified (or set to 0), the first meshtally in
the MCNP input is used.

Another modifications introduced with the patch is that two additional print
tables are printed to the ``outp`` file. The table ``print table FIS`` contains
material compositions for all materials in the input file in format suitable
for the FUEL fispact keyword. The table ``print table CMI`` contains cell
indices, names, material indices and names, and density and concentrations. 

After successfull run of the modified MCNP, files ``outcellsNN`` are generated.
The must be concatenated into a single file that later will be read by the
fispact driver. This can be done by running script `concatenate.sh`_ in the
folder where all `outcellsNN`` are located. This script generates file
``fine_mesh_content``, which contains all ``outcellsNN``. 

The fispact driver also needs information from the custom print tables
generated in the ``outp`` file.  Although one can specify that these tables
should be read directly from the `outp`` file, this file can be large and thus
it is better to extract the tables into separate files. This can be done with
`extract_tabls.sh`_ script, which accepts the name of the ``outp`` file as the
1-st command line argument. If the modified MCNP5 wrote output to ``outp``, the
command ::

    >extract_tables.sh outp


will extract ``print table CMI`` into file ``cmi_table``, and the ``print table
FIS`` into file ``mat_table``.    

.. _kit-materials.patch.5: ../mcnp-mod/kit-materials.patch.5
.. _concatenate.sh: ../scripts/concatenate.sh
.. _extract_tables.sh: ../scripts/extract_tables.sh

Files ``fine_mesh_content``, ``cmi_table``, ``mat_table`` generated with the
help of the modified MCNP, together with meshtal files containing neturon flux
intensity and spectra distributions (generated by unmodified MCNP after
modelling neutron transport) together describe material and neutron flux
disitribution necessary as input for the fispact driver.

MCNP patch for decay gamma transport
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the essence, modifications are needed only for the source sampling
subroutine. In details, however, some other subroutines are modified in order
to organize reading the decay gamma source file and preparing data for sampling
for each MPI process in an efficient way. The patch is `kit-gamma.patch.5`_. 

The modified MCNP reads from ``dgs`` file (its name is hardcoded in the patch)
the decay gamma source distribution -- gamma intensities and spectra in each
mesh element with activated material. For each mesh element, cumulative group
probabilites are computed and the data is disstributed among all MPI processes.

Details about decay gamma sampling are written in `decaygammasampling.rst`_.

.. _kit-gamma.patch.5: ../mcnp-mod/kit-gamma.patch.5
.. _decaygammasampling.rst: decaygammasampling.rst






