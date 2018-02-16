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

