{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Project where

-- TODO: don't forget to remove this
import ActivityOfEnhancements
import CodeWorld
import Data.Fixed
import Data.Maybe (listToMaybe)
import Data.Text (pack)
import Drawers
import System.Random
import Types

maxPassangers :: Int
maxPassangers = 30

drawGameState :: GameState -> Picture
drawGameState gameState =
  renderObject drawStation (getStations gameState)
    <> translated (-9) (-6) (lettering (pack $ show (getCurrentMode gameState)))
    <> translated (-9) (-4) (lettering (pack $ show (length (getRoutes gameState))))
    <> renderObject drawLocomotive (getLocomotives gameState)
    <> renderObject drawRoute (getRoutes gameState)
    <> backgroundImage

updateLocomotivePosition :: Double -> Locomotive -> Locomotive
updateLocomotivePosition dt (Locomotive route passengers direction progress) = Locomotive route passengers direction newProgress
  where
    newProgress = calculateProgress direction progress dt speed
    speed = 1 -- TODO: setup speed

calculateProgress :: Direction -> Double -> Double -> Double -> Double
calculateProgress Forward progress dt speed = progress + dt * speed
calculateProgress Backward progress dt speed = progress - dt * speed

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
withinError a b epsilon = abs (a - b) < epsilon

-- TODO: fix, with some speed doesn't transfer
-- TODO: get better check for transfer as I had to add 0.9 and 0.1 as starting pos to stop wrong transfers (maybe there is a need for better architecture)
transferLocomotive :: [Route] -> Locomotive -> Locomotive
transferLocomotive [] locomotive@(Locomotive route passengers direction progress)
  | progress > 1 && direction == Forward = Locomotive route passengers Backward 1.0
  | progress < 0 && direction == Backward = Locomotive route passengers Forward 0
  | otherwise = locomotive
transferLocomotive (first : rest) locomotive
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

withTimePassing :: forall world. Double -> Double -> (Double -> world -> world) -> (Double -> world -> world)
withTimePassing currentTime threshold func
  | currentTime `mod'` threshold < 0.05 = func
  | otherwise = const id

-- TODO: add exponential grow of appearance of new passengers
updateStation :: Double -> Station -> Station
updateStation _dt (Station stationType pos passengers gen) = Station stationType pos newPassengers newGen
  where
    (number, newGen) = randomR (0 :: Int, 2) gen
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
    newTime = dt + currentTime
    currentTime = getCurrentTime state

    updatedStations = map (withTimePassing currentTime 2 updateStation dt) stations
    newState =
      GameState
        updatedStations
        routes
        updatedLocomotives
        (getCurrentMode state)
        newTime

-- TODO: Creation of routes by point click
-- TODO: addition of locomotive by point and click
updateGameState :: Event -> GameState -> GameState
updateGameState (PointerPress p) state = handleClick p state
updateGameState (TimePassing dt) state = updateDynamic dt state
updateGameState _ state = state

getStationByCoord :: Point -> GameState -> Maybe Station
getStationByCoord p state = myStations
  where
    myStations :: Maybe Station
    myStations = listToMaybe $ filter (\a -> withinErrorPosition (getStationPosition a) p 1) (getStations state)

handleClick :: Point -> GameState -> GameState
handleClick point state@(GameState stations routes locos Play time) = turnConstructionOn
  where
    turnConstructionOn =
      case getStationByCoord point state of
        Nothing -> state
        Just x -> GameState stations routes locos (Construction (x)) time

handleClick point state@(GameState stations routes locos (Construction startStation) time) = turnConstructionOff
  where
    turnConstructionOff =
      case getStationByCoord point state of
        Nothing -> state
        Just x -> GameState stations newRoutes locos Play time
          where
            newRoutes :: [Route]
            newRoutes = Route brown (getStationPosition startStation) (getStationPosition x) : routes

handleClick _ (GameState stations routes locos mode time) = GameState stations routes locos mode time

initialSystem :: GameState
initialSystem =
  GameState
    [ Station Circle (3, 4) [] (mkStdGen 42),
      --  [Passenger Circle, Passenger Rectangle, Passenger Triangle, Passenger Circle, Passenger Rectangle, Passenger Triangle, Passenger Circle, Passenger Rectangle, Passenger Triangle, Passenger Circle,
      --  Passenger Rectangle, Passenger Triangle, Passenger Circle, Passenger Rectangle, Passenger Triangle ,Passenger Circle, Passenger Rectangle, Passenger Triangle ,Passenger Circle, Passenger Rectangle,
      --  Passenger Triangle ,Passenger Circle, Passenger Rectangle, Passenger Triangle ,Passenger Circle, Passenger Rectangle, Passenger Triangle, Passenger Circle, Passenger Rectangle, Passenger Triangle],
      Station Rectangle (2, -6) [] (mkStdGen 41),
      Station Triangle (-4, 2) [] (mkStdGen 40),
      Station Triangle (4, 2) [] (mkStdGen 40)
    ]
    [Route brown (3, 4) (2, -6), Route brown (2, -6) (-4, 2)]
    [Locomotive (Route brown (3, 4) (2, -6)) [] Forward 0.0]
    Play
    0

isGameOver :: GameState -> Bool
isGameOver state = stationsAreFull (getStations state)
  where
    stationsAreFull :: [Station] -> Bool
    stationsAreFull [] = False
    stationsAreFull (first : rest) = length (getStationPassengers first) >= maxPassangers || stationsAreFull rest

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
run = debugActivityOf initialSystem updateGameState drawGameState
