HOW TO DEPLOY
==================
The following recipe assumes that ``$WORK`` folder is writable. 

Create folder, where exicutables and default files will be copied, clone there the repository and prepare subfolders::

    > mkdir $WORK/r2smesh
    > cd $WORK/r2smesh
    
    > git clone git@github.com:cr2s/r2smesh-at.git
    > ln -s r2smesh-at/scripts .
    > ln -s r2smesh-at/files .
    > ln -s r2smesh-at/doc/recipe.rst .

    > mkdir bin

Compile ``adriver.exe`` and place link to ``bin`` ::

    > cd r2smesh-at/adriver
    > ./compile.sh
    > cd ../../bin
    > ln -s ../r2smesh-at/adriver/adriver.exe .

Compile modified versions of MCNP5 and place links to ``bin/``::

    > ln -s /path/to/mcnp5-md.mpi  .
    > ln -s /path/to/mcnp5-dg.mpi  .

    
