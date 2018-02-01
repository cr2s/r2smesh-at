# HOW TO DEPLOY
==================
The following recipe assumes that ``$WORK`` variable is defined and points to a place where the user can write.


Create folder, where exicutables and default files will be copied, clone there the repository and prepare subfolders::

    > mkdir $WORK/r2smesh
    > cd $WORK/r2smesh
    
    > git clone git@github.com:cr2s/dev.git
    > ln -s dev/kit-at/scripts .
    > ln -s dev/kit-at/files .
    > ln -s dev/kit-at/recipe.rst .

    > mkdir bin

Compile ``adriver.exe`` and place link to ``bin`` ::

    > cd dev/kit-at/adriver
    > ./compile.sh
    > cd ../../../bin
    > ln -s ../dev/kit-at/adriver/adriver.exe .

Compile modified varions of MCNP5 and place links to ``bin/``::

    > ln -s /path/to/mcnp5-md.mpi  .
    > ln -s /path/to/mcnp5-dg.mpi  .

    
