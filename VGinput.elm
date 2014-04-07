module VGinput where

import Mouse
import Keyboard
import Random
{--------------- INPUT -------------}
type Input = { flips:Int
             , mpos:(Int, Int)
             , space:Bool
             , rando:Time
             , delta:Time }

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
   
