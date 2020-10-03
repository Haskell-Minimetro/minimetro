module Project where

import CodeWorld
import Types

backgroundImage :: Picture
backgroundImage = drawStationType Triangle
  <> drawStationType Rectangle
  <> drawStationType Circle
  <> colored gray (solidRectangle 100 100)


drawStationType :: StationType -> Picture
drawStationType Triangle = 
  translated 0 0.05
    (scaled 0.9 0.9 (colored white triangle)) <> colored black triangle
  where
    triangle = solidPolygon [(-1,0), (0, 1.73), (1, 0)]
drawStationType Rectangle = translated 0 0
  (scaled 0.9 0.9 (colored white (solidRectangle 1 1))) <> colored black (solidRectangle 1 1)
drawStationType Circle = translated 0 0
  (scaled 0.9 0.9 (colored white (solidCircle 0.5))) <> colored black (solidCircle 0.5)

drawCurrentDate :: Double -> Picture
drawCurrentDate _time = blank

drawStation :: Station -> Picture
drawStation (Station stationType pos passangers) =
  drawStationType stationType <>
  drawPassengersOnStation pos passangers

drawPassengersOnStation :: Position -> [Passenger] -> Picture
drawPassengersOnStation _pos _passengers = blank

drawGameState :: GameState -> Picture
drawGameState _GameState = backgroundImage

updateDynamic :: Double -> GameState -> GameState
updateDynamic _dt state = state 

updateGameState :: Event -> GameState -> GameState
updateGameState (TimePassing dt) state = updateDynamic dt state 
updateGameState _ state = state 

run :: IO ()
run = activityOf GameState updateGameState drawGameState
