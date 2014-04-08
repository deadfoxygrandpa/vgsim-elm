module VGrender where

import Text
import VGmodel (..)

sty_n : Style
sty_n = { defaultStyle| color <- black}

sty_f : Style
sty_f = { defaultStyle| color <- white}

message : Color -> String -> Form
message c s = toText s  |> Text.color c
                        |> leftAligned
                        |> toForm

-- Display function. Takes everything and spits it out as an element
display : (Int, Int) -> Input -> GameState -> Element
display (w,h) 
        (inout)
        ({lstate, gstate} as game) = 
  let w' = toFloat w
      h' = toFloat h
      light = if  | lstate == Off -> rect w' h' |> filled black
                  | otherwise     -> rect w' h' |> filled white
      clr =   if  | lstate == Off -> white
                  | otherwise -> black
      str =   if  | gstate == Start -> "video game simulator\nby stepvhen"
                  | otherwise -> ""
   in flow down [
                 asText inout, asText game, 
                 collage w h [light, message clr str]
                 ]

