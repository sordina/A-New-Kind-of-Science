import Prelude hiding (Left,Right)
import OneDSimple

main = run "ANKoS - Rule 110" progression 200 700 Left

progression (False, False, False) = True
progression (False, False, True)  = False
progression (False, True,  False) = False
progression (False, True,  True)  = True
progression (True,  False, False) = False
progression (True,  False, True)  = False
progression (True,  True,  False) = False
progression (True,  True,  True)  = True
