HOW TO DEPLOY
==================

Steps to deploy:

   1. Clone the r2smesh repo

   2. Compile adriver

   3. Compile modified MCNP 

   4. Set R2S_ROOT variable and optionally FISPACT_DATA variable

Scheme to run a calculation:

   1. Create working place -- a folder containing ``input`` subfolder (see example)
      and optional `r2s_env_local.sh`.

   2. Source ``$R2S_ROOT/scripts/r2s_env.sh``. This is the "system-wide" script that
      sets all necessary variables for adriver to their default values. This script
      checks if the ``r2s_env_local.sh`` is found in the current folder and sources
      it as well (thus the user can redefine all default values). 

   3. Start adriver. Either interactively or using a job submission system.


