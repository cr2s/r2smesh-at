# R2Smesh
This is an implementation of the rigorous 2-step (R2S) method to calculate shutdown dose rates (SDDR) due to neutron irradiation of materials. The method comprises the following steps: (1) Simulation of neutron transport to obtain neutron flux spatial and energy distribution; (2) inventory calculations and activation analysis to obtain decay gamma source intensity and spactra as spatial distribution; (3) simulation of the decay gamma transport. The R2Smesh system presented in this repo organizes dataflow between the steps. 

There are several stand-alone codes to simulate neutron and gamma transport and to perform inventory calculations. This system uses MCNP5 for neutron and gamma transport and FISPACT-II for inventory calculations. 

The implementation in this repo is written from scratch, but in some aspects mimics the
R2Smesh developed in KIT/INR in 2005 (?) -- 2014.

A short recipe how to start calculations can be found in the
[recipe](doc/recipe.rst) and [deploy](/deploy.rst).


# Development
Feel free to propose a new feature by adding an issue with the `enhancement` label. As examples, look to already existing [proposals](https://github.com/cr2s/r2smesh-at/issues?q=is%3Aopen+is%3Aissue+label%3Aenhancement). Alternatively (e.g. if you don't have the github account), contact the development team directly: <r2smesh@gmail.com>
