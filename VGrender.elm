module VGrender where

import Graphics.Input as I
import Text
import VGmodel (..)

sty_n : Style
sty_n = { defaultStyle| color <- black}

sty_f : Style
sty_f = { defaultStyle| color <- white}

message : Color -> String -> Form
message c s = toText s  |> Text.color c
                        |> centered
                        |> toForm

modeButton : Color -> Color -> Int -> Int -> Gstate -> String -> Element
modeButton bg fg w h mode name =
  let btn clr = toText name |> Text.color fg 
                            |> centered
                            |> container w h middle
                            |> color clr
  in  I.customButton i_state.handle mode (btn bg) (btn gray) (btn darkGray)

title s = 
  let clr = if s == On then black else white
   in message clr "video game simulator\nby stepvhen"        

skl_btn s = 
  let bg = if s == Off then black else white
      fg = if s == Off then white else black
   in modeButton bg fg 73 20 Skill "skill"
              
sur_btn s = 
  let bg = if s == Off then black else white
      fg = if s == Off then white else black
   in modeButton bg fg 73 20 Survival "survive"
              
exp_btn s = 
  let bg = if s == Off then black else white
      fg = if s == Off then white else black
   in modeButton bg fg 73 20 Explore "explore"
              
snd_btn s = 
  let bg = if s == Off then black else white
      fg = if s == Off then white else black
   in modeButton bg fg 73 20 Sandbox "sandbox"
              
att_btn s = 
  let bg = if s == Off then black else white
      fg = if s == Off then white else black
   in modeButton bg fg 73 20 Attract "attract"
              
modeSelect : Lstate -> Form
modeSelect s = 
  [ title s |> moveY 80
  , skl_btn s |> toForm |> moveY  40 
  , sur_btn s |> toForm |> moveY  20
  , exp_btn s |> toForm 
  , snd_btn s |> toForm |> moveY -20
  , att_btn s |> toForm |> moveY -40
  ] |> group |> moveY -30
              
-- Display function. Takes everything and spits it out as an element
display : (Int, Int) -> Input -> GameState -> Element
display (w,h) 
        (inout)
        ({lstate, gstate} as game) = 
  let w' = toFloat w
      h' = toFloat h
      light = if  | lstate == Off -> rect w' h' |> filled black
                  | otherwise     -> rect w' h' |> filled white
   in flow down [
                 --asText inout, asText game, 
                 collage w h [light, (modeSelect lstate)]
                 ]

