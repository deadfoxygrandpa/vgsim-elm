module VGupdate where

import VGmodel (..)

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
      Start   -> if timed then toggle lstate else lstate
      Skill   -> if | lstate == Off && timed  -> toggle lstate
                    | lstate == On && flipped -> toggle lstate
                    | otherwise -> lstate
      Survival -> if | lstate == On  && timed   -> toggle lstate
                    | lstate == Off && flipped -> toggle lstate
                    | otherwise -> lstate
      Explore -> if | flipped -> toggle lstate
                    | otherwise -> lstate
      Sandbox -> if | timed   -> toggle lstate
                    | flipped -> toggle lstate
                    | otherwise -> lstate
      Attract -> if timed then toggle lstate else lstate
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
pauseGame ({ space, istate } as input) ({gstate, paused} as game) =
  let paused' = if space then (not paused) else paused
  in if | not paused' -> stepGame input {game| paused <- paused'} 
        | istate == Start -> stepGame input <| 
                  { game | gstate <- istate
                         , paused <- False
                  }
        | otherwise ->  {game| paused <- paused'}
     
-- Game state logic
stepGame : Input -> GameState -> GameState
stepGame { flips, space, rando, delta, istate }
         ({ lstate, gstate, paused, gflips, timer, grando, player } as game) =
    let state' = lightSwitch game (flips > gflips) (timer > grando)
        gstate' = istate
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
                     
