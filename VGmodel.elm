module VGmodel where

import Graphics.Input as I
import Mouse
import Keyboard
import Random
{--------------- INPUT -------------}
type Input = { flips:Int
             , mpos:(Int, Int)
             , space:Bool
             , rando:Time
             , delta:Time 
             , istate:Gstate }

i_state : I.Input Gstate
i_state = I.input Start

myfps = 20

tdelta : Signal Time
tdelta = inSeconds <~ fps myfps

randogen : Signal Time
randogen = Random.float randSig |> lift (\x -> (x * 4) + 0.51 )

randSig : Signal Time
randSig = merge (every second) <| toFloat <~ count Mouse.clicks

spacebar : Bool -> Bool -> Bool
spacebar hit after = hit && after

input : Signal Input
input = sampleOn tdelta <| 
  Input <~ count Mouse.clicks
         ~ Mouse.position
         ~ ((\x y -> x && y) <~ Keyboard.space 
                              ~ ((second / myfps) `since` Keyboard.space))
         ~ randogen
         ~ tdelta
         ~ i_state.signal
   
{------------------ MODEL --------------}
-- Game states, for implementing in the future. 
data Gstate 
  = Start 
  | Pause 
  | Skill 
  | Survival 
  | Explore 
  | Sandbox 
  | Attract

data Lstate = On | Off

type Player = 
  { timeOn:Time
  , timeOff:Time
  , pflips:Int }

defaultPlayer : Player
defaultPlayer = 
  { timeOn  = 0.0
  , timeOff = 0.0
  , pflips  = 0 }


type GameState = 
  { lstate:Lstate
  , gstate:Gstate
  , paused:Bool
  , gflips:Int
  , timer:Time
  , grando:Time
  , player:Player}
                 
defaultGame : GameState
defaultGame = 
  { lstate = On
  , gstate = Start
  , paused = False
  , gflips = 0
  , timer = 0.0
  , grando = 5.0 
  , player = defaultPlayer}


