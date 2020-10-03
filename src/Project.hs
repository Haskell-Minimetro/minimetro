{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Project where

import CodeWorld
import Types
import Data.Text (pack) -- TODO: don't forget to remove this
import ActivityOfEnhancements
import System.Random

maxPassangers :: Int
maxPassangers = 30

backgroundImage :: Picture
backgroundImage = colored (lighter 0.5 brown) (solidRectangle 100 100)

drawStationType :: StationType -> Picture
drawStationType Triangle =
  scaled 0.85 0.85 (colored white (drawFigure TriangleFigure))
  <> colored black (drawFigure TriangleFigure)
drawStationType Rectangle =
  scaled 0.85 0.85 (colored white (drawFigure SquareFigure))
  <> colored black (drawFigure SquareFigure)
drawStationType Circle =
  scaled 0.85 0.85 (colored white (drawFigure CircleFigure))
  <> colored black (drawFigure CircleFigure)

drawPassanger :: Passenger -> Picture
drawPassanger (Passenger Triangle) = scaled 0.2 0.2 (drawFigure TriangleFigure)
drawPassanger (Passenger Rectangle) = scaled 0.2 0.2 (drawFigure SquareFigure)
drawPassanger (Passenger Circle) = scaled 0.2 0.2 (drawFigure CircleFigure)

-- TODO: fix this triangle draw wrong
drawFigure :: Figure -> Picture
drawFigure TriangleFigure = triangle
  where
    triangle = solidPolygon [(-triangleSize/2, -(median-radius)), (0, radius), (triangleSize/2, -(median-radius))]
    triangleSize = 1.2
    radius = triangleSize * sqrt 3/3
    median = sqrt 3 * triangleSize / 2

drawFigure SquareFigure = solidRectangle 1 1
drawFigure CircleFigure = solidCircle 0.5

drawCurrentDate :: Double -> Picture
drawCurrentDate _time = blank

drawStation :: Station -> Picture
drawStation (Station stationType (x, y) passangers) 
  = translated x y (drawStationType stationType) <> drawPassengersOnStation (x, y) passangers


drawInARow :: [a] -> Double -> (a->Picture) -> Picture
drawInARow [] _ _ = blank
drawInARow (first:rest) margin drawer = 
  drawer first
  <> translated margin 0 (drawInARow rest margin drawer)

drawPassengersOnStation :: Position -> [Passenger] -> Picture
drawPassengersOnStation (x, y) passengers = translated (x+0.5) (y+0.5) (passes passengers)
  where
    passes :: [Passenger] -> Picture
    passes [] = blank
    passes passenger =drawInARow (take 10 passenger) 0.25 drawPassanger <> translated 0 0.25 (passes (drop 10 passenger))

renderStations :: [Station] -> Picture
renderStations = pictures . map drawStation 

renderRoutes :: [Route] -> Picture
renderRoutes = pictures . map drawRoute

renderLocomotives :: [Locomotive] -> Picture
renderLocomotives = pictures . map drawLocomotive

drawGameState :: GameState -> Picture 
drawGameState gameState =
  renderStations (getStations gameState)
  <> renderLocomotives (getLocomotives gameState)
  <> renderRoutes (getRoutes gameState)
  <> backgroundImage

drawRoute :: Route -> Picture
drawRoute (Route color station1 station2) = colored color (thickCurve 0.3 positions)
  where
    positions = [station1, station2]

-- TODO: remove debug stuff
drawLocomotive :: Locomotive -> Picture
drawLocomotive locomotive = translated x y (rotated (-angle) (passengers <> locomotiveBase))
  <> translated (-9) (-9) (lettering (pack $ show progress))
  <> translated (-9) (-8) (lettering (pack $ show $ getLocomotiveDirection locomotive))
  <> translated (-9) (-7) (lettering (pack $ show pos1))
  where
    angle = pi/2 - getAngle pos1 pos2
    progress = getLocomotivePosition locomotive
    (x, y) = getTrainPositionWithProgress progress pos1 pos2

    (Route locomotiveColor pos1 pos2) = getLocomotiveRoute locomotive
    locomotiveBase = colored (light locomotiveColor) (solidRectangle 0.66 1)
    passengers = translated (-0.13) (-0.25) (colored (lighter 0.4 locomotiveColor) (drawPassengersOnTrain (getLocomotivePassengers locomotive)))

getTrainPositionWithProgress :: Double -> Position -> Position -> Position
getTrainPositionWithProgress percent (x1,y1) (x2,y2) = (x1*(1-percent)+x2*percent, y1*(1-percent)+y2*percent)

getAngle :: Position -> Position -> Double
getAngle (x1, y1) (x2, y2) = atan ((y1-y2)/(x1-x2))

drawPassengersOnTrain :: [Passenger] -> Picture
drawPassengersOnTrain [] = blank
drawPassengersOnTrain passenger = drawInARow (take 2 passenger) 0.25 drawPassanger <> translated 0 0.25 (drawPassengersOnTrain (drop 2 passenger))

updateLocomotivePosition :: Double -> Locomotive -> Locomotive
updateLocomotivePosition dt (Locomotive route passengers direction progress) = Locomotive route passengers direction newProgress
  where
    newProgress = calculateProgress direction progress dt speed
    speed = 1 -- TODO: setup speed

calculateProgress :: Direction -> Double -> Double -> Double -> Double
calculateProgress Forward progress dt speed = progress + dt*speed
calculateProgress Backward progress dt speed = progress - dt*speed

inverseDirection :: Direction -> Direction
inverseDirection Backward = Forward
inverseDirection Forward = Backward


checkTransfer :: Position -> Route -> (Bool, Direction)
checkTransfer trainPosition (Route _ pos1 pos2)
  | withinErrorPosition trainPosition pos1 1e-0 = (True, Forward)
  | withinErrorPosition trainPosition pos2 1e-0 = (True, Backward)
  | otherwise = (False, Forward)


withinErrorPosition :: Position -> Position -> Double -> Bool
withinErrorPosition (x, y) (x2, y2) epsilon = withinError x x2 epsilon && withinError y y2 epsilon

withinError :: Double -> Double -> Double -> Bool
withinError a b epsilon = abs (a-b) < epsilon

-- TODO: fix, with some speed doesn't transfer
-- TODO: get better check for transfer as I had to add 0.9 and 0.1 as starting pos to stop wrong transfers (maybe there is a need for better architecture)
transferLocomotive :: [Route] -> Locomotive -> Locomotive
transferLocomotive [] locomotive@(Locomotive route passengers direction progress)
  | progress > 1 && direction == Forward = Locomotive route passengers Backward 1.0
  | progress < 0 && direction == Backward = Locomotive route passengers Forward 0
  | otherwise = locomotive
transferLocomotive (first:rest) locomotive
  | color1 == color2 && canGo && (routePos1 /= pos1) && (routePos2 /= pos2) = newLocomotive
  | otherwise = transferLocomotive rest locomotive
    where 
      (canGo, direction) = checkTransfer currentPosition first
      
      whereToStart Backward = 0.9
      whereToStart Forward = 0.1

      newLocomotive = Locomotive first (getLocomotivePassengers locomotive) direction (whereToStart direction)
      
      (Route color1 routePos1 routePos2) = first
      (Route color2 pos1 pos2) = getLocomotiveRoute locomotive 
      currentPosition = getTrainPositionWithProgress (getLocomotivePosition locomotive) pos1 pos2

-- data TimePassed world = TimePassed Double world
-- withTimePassing :: Double -> (Double -> TimePassed world -> TimePassed world) -> (Double -> world -> world)
-- withTimePassing threshold updateWorld = newUpdateWorld
--   where
--     newUpdateWorld time (TimePassed accumulator state)
--       | if accumulator > threshold = updateWorld time
--       | otherwise = 

-- TODO: add randomness to passenger addition
-- TODO: add exponential grow of appearance of new passengers
updateStation :: Double -> Station -> Station
updateStation _dt (Station stationType pos passengers) = Station stationType pos newPassengers
  where
    (number, _newGen) = randomR (0 :: Int, 2) (mkStdGen 9)

    newPassengers = passengers ++ [getRandomPassenger number]


getRandomPassenger :: Int -> Passenger
getRandomPassenger 0 = Passenger Triangle
getRandomPassenger 1 = Passenger Rectangle
getRandomPassenger _ = Passenger Circle

-- TODO: passanger setting on the train
updateDynamic :: Double -> GameState -> GameState
updateDynamic dt state = newState
  where
    stations = getStations state
    routes = getRoutes state
    locomotives = getLocomotives state
    updatedLocomotives = map (transferLocomotive routes . updateLocomotivePosition dt) locomotives -- . filter (\(Locomotive _ _ _ progress) -> withinError progress 1.0 1e-1 || withinError progress 0.0 1e-1)


    updatedStations = map (updateStation dt) stations
    newState = GameState
                updatedStations
                routes
                updatedLocomotives

-- TODO: Creation of routes by point click
-- TODO: addition of locomotive by point and click
updateGameState :: Event -> GameState -> GameState
updateGameState (TimePassing dt) state = updateDynamic dt state 
updateGameState _ state = state 

initialSystem :: GameState
initialSystem = GameState [
    Station Circle (3, 4) [],
                          --  [Passenger Circle, Passenger Rectangle, Passenger Triangle, Passenger Circle, Passenger Rectangle, Passenger Triangle, Passenger Circle, Passenger Rectangle, Passenger Triangle, Passenger Circle,
                          --  Passenger Rectangle, Passenger Triangle, Passenger Circle, Passenger Rectangle, Passenger Triangle ,Passenger Circle, Passenger Rectangle, Passenger Triangle ,Passenger Circle, Passenger Rectangle,
                          --  Passenger Triangle ,Passenger Circle, Passenger Rectangle, Passenger Triangle ,Passenger Circle, Passenger Rectangle, Passenger Triangle, Passenger Circle, Passenger Rectangle, Passenger Triangle],
    Station Rectangle (2, -6) [],
    Station Triangle (-4, 2) []
  ]
  [Route brown (3,4) (2, -6), Route brown (2, -6) (-4, 2)]
  [Locomotive (Route brown (3,4) (2, -6)) [] Forward 0.0]

isGameOver :: GameState -> Bool
isGameOver state = stationsAreFull (getStations state)
  where
    stationsAreFull :: [Station] -> Bool
    stationsAreFull [] = False
    stationsAreFull (first:rest) = length (getStationPassengers first) > maxPassangers || stationsAreFull rest

-- TODO: Maybe some better showage that its over
withGameOver :: forall world. (world -> Bool) -> ActivityOf world -> ActivityOf world
withGameOver isOver originalActivityOf userWorld userHandler userDrawer =
  originalActivityOf userWorld ignoreInput updatedDrawer
  where
    ignoreInput :: Event -> world -> world
    ignoreInput event state
      | isOver state = state
      | otherwise = userHandler event state

    updatedDrawer :: world -> Picture
    updatedDrawer state
      | isOver state = translated 9 9 (lettering "Game Over") <> userDrawer state
      | otherwise = userDrawer state
    
  
run2 :: IO ()
run2 = withGameOver isGameOver (withStartScreen (withReset activityOf)) initialSystem updateGameState drawGameState

run :: IO ()
run = activityOf initialSystem updateGameState drawGameState
