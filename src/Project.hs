{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Project where

import ActivityOfEnhancements
import CodeWorld
import Drawers
import System.Random
import Data.Fixed
import Types

maxPassangers :: Int
maxPassangers = 30

maxPassengersOnTrain :: Int
maxPassengersOnTrain = 6

drawGameState :: GameState -> Picture
drawGameState gameState =
  renderObject drawStation (getStations gameState)
    <> renderObject drawLocomotive (getLocomotives gameState)
    <> renderObject drawRoute (getRoutes gameState)
    <> backgroundImage

updateLocomotivePosition :: Double -> Locomotive -> Locomotive
updateLocomotivePosition dt (Locomotive passengers direction (OnRoute route progress)) = Locomotive passengers direction (OnRoute route newProgress)
  where
    newProgress = calculateProgress direction progress dt speed
    speed = 1 -- TODO: setup speed
updateLocomotivePosition _dt locomotive = locomotive

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

-- -- TODO: fix, with some speed doesn't transfer
-- -- TODO: get better check for transfer as I had to add 0.9 and 0.1 as starting pos to stop wrong transfers (maybe there is a need for better architecture)
-- transferLocomotive :: [Route] -> Locomotive -> Locomotive
-- transferLocomotive [] locomotive@(Locomotive passengers direction (OnRoute route progress))
--   | progress > 1 && direction == Forward = Locomotive passengers Backward (OnRoute route 1.0)
--   | progress < 0 && direction == Backward = Locomotive passengers Forward (OnRoute route 0.0)
--   | otherwise = locomotive
-- transferLocomotive (first : rest) locomotive
--   | color1 == color2 && canGo && (routePos1 /= pos1) && (routePos2 /= pos2) = newLocomotive
--   | otherwise = transferLocomotive rest locomotive
--   where
--     (canGo, direction) = checkTransfer currentPosition first

--     whereToStart Backward = 0.9
--     whereToStart Forward = 0.1

--     newLocomotive = Locomotive (getLocomotivePassengers locomotive) direction (OnRoute first (whereToStart direction))

--     (Route color1 routePos1 routePos2) = first
--     (OnRoute (Route color2 pos1 pos2) progress) = getLocomotiveStatus locomotive
--     currentPosition = getTrainPositionWithProgress progress pos1 pos2
-- transferLocomotive _ locomotive = locomotive

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

-- TODO: the state of the station may change for transfers, but not for now
transferPassangersToStation :: Locomotive -> Station -> (Locomotive, Station)
transferPassangersToStation (Locomotive passengers direction (TransferTo position color)) station = (updatedLocomotive, updatedStation)
  where
    trainPassangers = map Passenger (filter (==stationType) (map (\(Passenger x) -> x) passengers))
    stationType = getStationType station

    updatedLocomotive = Locomotive trainPassangers direction (TransferFrom position color)
    updatedStation = station
transferPassangersToStation locomotive station = (locomotive, station)

transferPassangersToLocomotive :: Locomotive -> Station -> (Locomotive, Station)
transferPassangersToLocomotive (Locomotive trainPassangers direction (TransferFrom position color)) station = (updatedLocomotive, updatedStation)
  where
    stationPassengers = getStationPassengers station
    maxToTransfer = 6 - length trainPassangers

    newTrainPassangers = trainPassangers ++ take maxToTransfer stationPassengers
    newStationsPassengers = drop maxToTransfer stationPassengers

    updatedLocomotive = Locomotive newTrainPassangers direction (Ready position color)
    updatedStation = Station (getStationType station) (getStationPosition station) newStationsPassengers (getPassengerGen station)
transferPassangersToLocomotive locomotive station = (locomotive, station)

transferLocomotive :: [Route] -> Locomotive -> Locomotive
transferLocomotive [] locomotive@(Locomotive _ _ (Ready _ _)) = locomotive -- Go backwards
transferLocomotive (first: rest) locomotive@(Locomotive passagners direction (Ready pos color1))
  | color1 == color2 = newLocomotive
  | otherwise = transferLocomotive rest locomotive
    where
      newLocomotive = Locomotive (getLocomotivePassengers locomotive) direction (OnRoute first (whereToStart direction))
    -- | color1 == color2 && canGo && (routePos1 /= pos1) && (routePos2 /= pos2) = newLocomotive
    -- | otherwise = transferLocomotive rest locomotive
    -- where
    --   (canGo, direction) = checkTransfer currentPosition first

    --   whereToStart Backward = 0.9
    --   whereToStart Forward = 0.1

    --   newLocomotive = Locomotive (getLocomotivePassengers locomotive) direction (OnRoute first (whereToStart direction))

    --   (Route color1 routePos1 routePos2) = first
    --   (OnRoute (Route color2 pos1 pos2) progress) = getLocomotiveStatus locomotive
    --   currentPosition = getTrainPositionWithProgress progress pos1 pos2


-- TODO: passanger setting on the train
updateDynamic :: Double -> GameState -> GameState
updateDynamic dt state = newState
  where
    stations = getStations state
    routes = getRoutes state
    locomotives = getLocomotives state
    updatedLocomotives = map ( updateLocomotivePosition dt . transferLocomotive routes) locomotives -- . filter (\(Locomotive _ _ _ progress) -> withinError progress 1.0 1e-1 || withinError progress 0.0 1e-1)
    --  transferPassangersFromStation . transferPassangersToStation 
    newTime = dt + currentTime
    currentTime = getCurrentTime state

    updatedStations = map (withTimePassing currentTime 2 updateStation dt) stations
    newState = GameState
                updatedStations
                routes
                updatedLocomotives
                newTime

-- TODO: Creation of routes by point click
-- TODO: addition of locomotive by point and click
updateGameState :: Event -> GameState -> GameState
updateGameState (TimePassing dt) state = updateDynamic dt state
updateGameState _ state = state

initialSystem :: GameState
initialSystem = GameState [
    Station Circle (3, 4) [] (mkStdGen 42),
                          --  [Passenger Circle, Passenger Rectangle, Passenger Triangle, Passenger Circle, Passenger Rectangle, Passenger Triangle, Passenger Circle, Passenger Rectangle, Passenger Triangle, Passenger Circle,
                          --  Passenger Rectangle, Passenger Triangle, Passenger Circle, Passenger Rectangle, Passenger Triangle ,Passenger Circle, Passenger Rectangle, Passenger Triangle ,Passenger Circle, Passenger Rectangle,
                          --  Passenger Triangle ,Passenger Circle, Passenger Rectangle, Passenger Triangle ,Passenger Circle, Passenger Rectangle, Passenger Triangle, Passenger Circle, Passenger Rectangle, Passenger Triangle],
    Station Rectangle (2, -6) [] (mkStdGen 41),
    Station Triangle (-4, 2) [] (mkStdGen 40)
  ]
  [Route brown (3,4) (2, -6), Route brown (2, -6) (-4, 2)]
  [Locomotive [] Forward (Ready (3,4) brown)]
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
run = activityOf initialSystem updateGameState drawGameState
