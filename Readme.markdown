Worked Examples from Wolfram's "A New Kind of Science"
======================================================

A playful attempt at reproducing the 'rules' from ANKoS in Haskell and OpenGL.

Example
-------

    import Prelude hiding (Left,Right)
    import OneDSimple

    main = run progression 200 700 Center

    progression (False, False, False) = True
    progression (False, False, True)  = False
    progression (False, True,  False) = False
    progression (False, True,  True)  = True
    progression (True,  False, False) = False
    progression (True,  False, True)  = False
    progression (True,  True,  False) = False
    progression (True,  True,  True)  = True

Shortened Example
-----------------

The following example uses the shortcut notation for defining rules.
It is equivilant to the first example.


    import Utility

    main = runFromString "t  t   t"


After this, all that is needed is to compile and run the program.
This should yield a scrolling OpenGL window.


Running
-------

* Push any key to pause and resume scrolling


Known Bugs:
-----------

* The edge conditions of the rows are a bit iffy at the moment
* There seems to be some issues with alignments
* Performance is bad
