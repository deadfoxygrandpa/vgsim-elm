import VGmodel (..)
import VGupdate (..)
import VGrender (..)

import Window

gameState = foldp pauseGame defaultGame input
main = display <~ Window.dimensions ~ gameState -- ~ input
