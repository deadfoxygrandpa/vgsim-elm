module VGmodel where

{------------------ MODEL --------------}
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


