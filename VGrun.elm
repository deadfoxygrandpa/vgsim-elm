import VGinput (..)
import VGmodel (..)
import VGupdate (..)
import VGrender (..)

gameState = foldp pauseGame defaultGame input
main = display (300,300) <~ input ~ gameState
