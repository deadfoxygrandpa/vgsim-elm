import Mouse
import Random
import Window

{--------------- INPUT -------------}
type Input = { flips:Int
             , mpos:(Int, Int)
             , rando:Time
             , delta:Time }

diffTime : Signal a -> Signal b -> Signal Float
diffTime a b = let f x =( inSeconds . fst) <~ timestamp x
               in  (-) <~ (f a) ~ (f b)

tdelta : Signal Time
tdelta = inSeconds <~ fps 30

randogen : Signal Time
randogen = Random.float randSig |> lift (\x -> (x * 4) + 0.51 )

randSig : Signal Time
randSig = merge (every second) (toFloat <~ count Mouse.clicks)

input : Signal Input
input = sampleOn tdelta <| 
  Input <~ count Mouse.clicks
         ~ Mouse.position
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
                 , gflips:Int
                 , timer:Time
                 , grando:Time
                 , player:Player}
                 
defaultGame : GameState
defaultGame = { lstate = On
              , gstate = Start
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
    let timeOn' = if | lstate == On   -> timeOn + delta
                     | otherwise      -> timeOn
        timeOff'= if | lstate == Off  -> timeOff + delta
                     | otherwise      -> timeOff
        pflips' = if flipped then pflips + 1 else pflips
     in { player | timeOn  <- timeOn'
                 , timeOff <- timeOff'
                 , pflips  <- pflips' }

-- Game state logic
stepGame : Input -> GameState -> GameState
stepGame { flips, mpos, rando, delta }
         ({ lstate, gstate, gflips, timer, grando, player } as game) =
    let state' = lightSwitch game (flips > gflips) (timer > grando)
        gstate' = Start
        gflips' = flips      
        timer' = if state' /= lstate then 0     else timer + delta
        grando'= if state' /= lstate then rando else grando
        player'= if gstate' /= gstate 
                 then defaultPlayer
                 else stepPlayer player lstate delta (state' /= lstate)       
    in {game | lstate <- state'
             , gstate <- gstate'
             , gflips <- gflips'
             , timer  <- timer'
             , grando <- grando'
             , player <- player'}
                     
{------------------------- RENDER ---------------------}

-- Display function. Takes everything and spits it out as an element
display : (Int, Int) -> Input -> GameState -> Element
display (w,h) 
        (inout)
        ({lstate} as game) = 
  let light = if | lstate == Off -> square 300 |> filled black
                 | otherwise     -> square 300 |> filled white
   in flow down [
                 asText inout, asText game, 
                 collage 300 300 [light]
                 ]

gameState = foldp stepGame defaultGame input
main = lift3 display Window.dimensions input gameState
