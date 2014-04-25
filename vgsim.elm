import VGmodel (..)
import VGupdate (..)
import VGrender (..)

import Window

gameState = foldp pauseGame defaultGame input
modeSelectForm = modeSelect <~ (dropRepeats <| .lstate <~ gameState)
main = display <~ Window.dimensions ~ modeSelectForm ~ gameState -- ~ input
