Define contribution of activated components to SDDR
=====================================================

Consider that several components are activated and thus make contribution to 
SDDR. Often question arises: what is the contirbution of each component? 

MCNP does not have capability to distinguish contribution of tracks started in different cells. Modifications
to the code are questionable. 

One can supply a list of cells to the driver. This will require additional code
to be written: new module describing the reader and arrays to contain cell
names.

Another option -- a pyhton filter that modifies the fine_mesh_content file
according to the list of cells: cells not in the list are removed (their
materials are replaced with zeroes). The same trick -- setting materials to
zero -- cannot be applied in gamma transport, therefore a list of cells where
photons start mast be supplied to the modified MCNP as well. This option seems
to be the simpliest to implement.

