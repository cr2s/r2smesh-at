The TODO list
================

* (From presentation 5.02.18) How the user can be confident that calculation is going fine?

* (From presentation 5.02.18) Is it possible to use another neutron energy group structure?

* Add procedures to check that all necessary files exist.

* Relation to cr2s repository. Originally, files for this implementation were
  stored as a subfolder in the cr2s repo. This was done with purpose: the cr2s
  repo contains patch scripts, some common patches (for fpp preprocessor, for
  torus) and the compilation script. These files are also needed here. How
  these files can be shared between two private repositories? 
   
  One possible solution is to extract all common files into a public repository (including patches to MCNP?).
   
  Another solution is to put here independent copies and let them diverge.
   

* Possibly, bug in `compile.sh`: it deletes first the mcnp executable, and than
  starts compilation with make. In case make does not find any new source file,
  it does not perform compilation. These two steps results in deleting the
  existing mcnp executable without generating a new one.  
