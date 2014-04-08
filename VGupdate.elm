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
                     
