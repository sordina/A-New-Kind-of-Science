import Prelude hiding (Left,Right)
import OneDSimple

main = run "ANKoS - Rule 30" progression 500 700 Center

progression (False, False, False) = True
progression (False, False, True)  = True
progression (False, True,  False) = True
progression (False, True,  True)  = False
progression (True,  False, False) = False
progression (True,  False, True)  = False
progression (True,  True,  False) = False
progression (True,  True,  True)  = True
