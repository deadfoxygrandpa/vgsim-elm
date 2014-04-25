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
  let btn b' f' = toText name |> Text.color f' 
                              |> centered
                              |> container w h middle
                              |> color b'
  in  I.customButton i_state.handle mode (btn bg fg) (btn gray black) (btn
  darkGray black)

title_btn bg fg = 
   modeButton bg fg 158 40 Start "video game simulator\nby stepvhen"        

skl_btn bg fg = 
   modeButton bg fg 73 20 Skill "skill"
              
sur_btn bg fg = 
   modeButton bg fg 73 20 Survival "survive"
              
exp_btn bg fg = 
   modeButton bg fg 73 20 Explore "explore"
              
snd_btn bg fg = 
   modeButton bg fg 73 20 Sandbox "sandbox"
              
att_btn bg fg = 
   modeButton bg fg 73 20 Attract "attract"
              
pause_title bg fg = 
  modeButton bg fg 98 40 Start "pause menu\nreturn to title?"

pauseMenu : Lstate -> Form
pauseMenu s = 
  let bg = if s == On then white else black
      fg = if s == On then black else white
   in pause_title bg fg |> toForm

modeSelect : Lstate -> Form
modeSelect s = 
  let bg = if s == On then white else black
      fg = if s == On then black else white
   in [ title_btn bg fg |> toForm |> moveY 80
      , skl_btn bg fg |> toForm |> moveY  40 
      , sur_btn bg fg |> toForm |> moveY  20
      , exp_btn bg fg |> toForm 
      , snd_btn bg fg |> toForm |> moveY -20
      , att_btn bg fg |> toForm |> moveY -40
      ] |> group |> moveY -30
              
-- Display function. Takes everything and spits it out as an element
display : (Int, Int) -> Form -> GameState -> Element
display (w,h) 
        --(inout)
        modeSelectForm
        ({lstate, gstate, paused} as game) = 
  let w' = toFloat w
      h' = toFloat h
      light = if  | lstate == Off -> rect w' h' |> filled black
                  | otherwise     -> rect w' h' |> filled white
      objects = if | gstate == Start -> light :: modeSelectForm :: []
                   | paused -> light :: pauseMenu lstate :: []
                   | otherwise -> light :: []
   in flow down [
                 --asText inout, asText game, 
                 collage w h objects
                 ]

