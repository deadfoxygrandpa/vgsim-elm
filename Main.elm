import Mouse
import Keyboard
import Random
import Window
import Text

{--------------- INPUT -------------}
type Input = { flips:Int
             , mpos:(Int, Int)
             , space:Bool
             , rando:Time
             , delta:Time }

tdelta : Signal Time
tdelta = inSeconds <~ fps 30

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
                              ~ ((second *0.03) `since` Keyboard.space))
         ~ randogen
         ~ tdelta
   
{------------------------- MODEL --------------}

-- Game states, for implementing in the future. 
data Gstate = Start 
            | Pause 
            | Skill 
            | Survival 
            | Explore 
            | Sandbox 
            | Attract

data Lstate = On | Off

type Player = { timeOn:Time
              , timeOff:Time
              , pflips:Int }

defaultPlayer : Player
defaultPlayer = { timeOn = 0.0
                , timeOff = 0.0
                , pflips = 0 }


type GameState = { lstate:Lstate
                 , gstate:Gstate
                 , paused:Bool
                 , gflips:Int
                 , timer:Time
                 , grando:Time
                 , player:Player}
                 
defaultGame : GameState
defaultGame = { lstate = On
              , gstate = Start
              , paused = False
              , gflips = 0
              , timer = 0.0
              , grando = 5.0 
              , player = defaultPlayer}


{---------------------- UPDATE -----------------}

-- Utility function. toggles a lgiht state
toggle : Lstate -> Lstate
toggle l = if | l == On -> Off
              | l == Off -> On
              | otherwise -> l
{--
  Light state changing logic 
  The bulk of the game. Manages light state based on
  the current light state, the game state, and whether 
  anything has been clicked or the timer has run out
--}
lightSwitch : GameState -> Bool -> Bool -> Lstate
lightSwitch {lstate, gstate} flipped timed = 
  if (not timed) && (not flipped) then lstate else
    case gstate of
      Start -> if timed then toggle lstate else lstate
      Skill -> if | lstate == Off && timed  -> toggle lstate
                  | lstate == On && flipped -> toggle lstate
                  | otherwise -> lstate
      _     -> toggle lstate

-- Method for recording game progress. 
stepPlayer : Player -> Lstate -> Time -> Bool -> Player
stepPlayer ({ timeOn, timeOff, pflips } as player)
           lstate delta flipped =
    let timeOn' = if lstate == On  then timeOn + delta else timeOn
        timeOff'= if lstate == Off then timeOff + delta else timeOff
        pflips' = if flipped then pflips + 1 else pflips
     in { player | timeOn  <- timeOn'
                 , timeOff <- timeOff'
                 , pflips  <- pflips' }

-- Primary update layer, allows for pausing of game. 
pauseGame : Input -> GameState -> GameState
pauseGame ({ space } as input) ({paused} as game) =
  let paused' = if space then (not paused) else paused
  in if (not paused') 
     then stepGame input {game| paused <- paused'} 
     else {game| paused <- paused'}
     
-- Game state logic
stepGame : Input -> GameState -> GameState
stepGame { flips, mpos, space, rando, delta }
         ({ lstate, gstate, paused, gflips, timer, grando, player } as game) =
    let state' = lightSwitch game (flips > gflips) (timer > grando)
        gstate' = gstate
        gflips' = flips      
        timer' = if state' /= lstate then 0     else timer + delta
        grando'= if state' /= lstate then rando else grando
        player'= if gstate' /= gstate 
                 then defaultPlayer
                 else stepPlayer player lstate delta (state' /= lstate) 
     in {game| lstate <- state'
             , gstate <- gstate'
             , gflips <- gflips'
             , timer  <- timer'
             , grando <- grando'
             , player <- player'}
                     
{------------------------- RENDER ---------------------}

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
                 --asText inout, asText game, 
                 collage w h [light, message clr str]
                 ]

{-------------------------- RUNTIME --------------------}

gameState = foldp pauseGame defaultGame input
main = lift3 display Window.dimensions input gameState
