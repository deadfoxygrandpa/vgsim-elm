import Graphics.Input as I

data Keys = Number Int | Plus | Minus | Clear

keys : I.Input Keys
keys = I.input Clear

calculator : Element
calculator = 
  flow right  [ I.button keys.handle (Number 1) "1"
              , I.button keys.handle (Number 2) "2"
              , I.button keys.handle Plus       "+"
              ]

main = calculator
