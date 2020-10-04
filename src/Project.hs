{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Project where

import ActivityOfEnhancements
import CodeWorld
import Data.Fixed
import Data.Text (pack)
import Drawers
import System.Random
import Types
import Data.List (find)
import Data.Maybe (listToMaybe)
import Config (maxPassangers)

drawGameState :: GameState -> Picture
drawGameState gameState =
  renderObject drawStation (getStations gameState)
    <> translated (-9) (-6) (lettering (pack $ show (getCurrentMode gameState)))
    -- <> translated (-9) (-4) (lettering (pack $ show (length (getRoutes gameState))))
    <> translated (-2) (-8) (drawAssets gameState)
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



-- updateAssets :: Double -> [Asset] -> [Asset]
-- updateAssets dt assets = newAssets
--   where
--     week = dt `div` 24 * 7
--     newAssets = if isNewWeek then do    
--         gen <- newStdGen
--         (number, _) <- randomR (0 :: Int, 2) gen 
--         return [LineColor (getRandomColor num) IsUsed False, Train IsUsed False] ++ assets
--       else return assets

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


routeFilter :: Color -> Position -> Route -> Bool
routeFilter color pos (Route routeColor pos1 _pos2) = color == routeColor && pos1 == pos

getTransferRoute :: [Route] -> Locomotive -> Maybe (Route, Direction)
getTransferRoute routes (Locomotive _ direction (Ready pos color)) =
  case direction of
    Forward ->
      case getTransferForward of
        Just routeForward -> Just (routeForward, Forward)
        Nothing -> case getTransferBackward of
                    Just routeBackward -> Just (routeBackward, Backward)
                    Nothing -> Nothing
    Backward ->
      case getTransferBackward of
        Just routeForward -> Just (routeForward, Backward)
        Nothing -> case getTransferForward of
                    Just routeBackward -> Just (routeBackward, Forward)
                    Nothing -> Nothing
  where
    getTransferForward :: Maybe Route
    getTransferForward = find (\(Route routeColor pos1 _pos2) -> color == routeColor && pos1 == pos) routes

    getTransferBackward ::Maybe Route
    getTransferBackward = find (\(Route routeColor _pos1 pos2) -> color == routeColor && pos2 == pos) routes
getTransferRoute _ _ = Nothing

transferLocomotive :: [Route] -> Locomotive -> Locomotive
transferLocomotive routes locomotive@(Locomotive passagners _ (Ready _ _)) = newLocomotive
  where
    whereToStart Backward = 1.0
    whereToStart Forward = 0.0
    newLocomotive = 
      case getTransferRoute routes locomotive of
        Just (newRoute, newDirection) -> Locomotive passagners newDirection (OnRoute newRoute (whereToStart newDirection))
        Nothing -> locomotive
transferLocomotive _routes locomotive = locomotive

directionRouteToPos :: Route -> Direction -> Position
directionRouteToPos (Route _ _ pos2) Forward = pos2
directionRouteToPos (Route _ pos1 _) Backward = pos1


stopLocomotive :: [Station] -> Locomotive -> Locomotive
stopLocomotive stations locomotive@(Locomotive passengers direction (OnRoute route@(Route color _ _) progress))
  | progress > 1 = 
    case getStationByCoord (directionRouteToPos route direction) stations of
      Just station -> Locomotive passengers direction (Ready (getStationPosition station) color) -- ToDo: TransferTo instead of Ready
      Nothing -> locomotive
  | progress < 0 = 
    case getStationByCoord (directionRouteToPos route direction) stations of
      Just station -> Locomotive passengers direction (Ready (getStationPosition station) color) -- ToDo: TransferTo instead of Ready
      Nothing -> locomotive
  | otherwise = locomotive
stopLocomotive _stations locomotive = locomotive

updateTime :: Double -> Double -> GameMode -> Double
updateTime currentTime dt Play = currentTime + dt
updateTime currentTime _ _ = currentTime

-- TODO: passanger setting on the train
updateDynamic :: Double -> GameState -> GameState
updateDynamic dt (GameState stations routes locomotives assets mode currentTime) = newState
  where
    updatedLocomotives = map (stopLocomotive stations . updateLocomotivePosition dt . transferLocomotive routes) locomotives -- Some filter on routes/locomotives
    -- TODO: transferPassangersFromStation . transferPassangersToStation 
    newTime = updateTime currentTime dt mode

    updatedStations = map (withTimePassing currentTime 2 updateStation dt) stations
    newState = GameState updatedStations routes updatedLocomotives assets mode newTime

-- TODO: Creation of routes by point click
-- TODO: addition of locomotive by point and click
updateGameState :: Event -> GameState -> GameState
updateGameState (PointerPress p) state = handleClick p state
updateGameState (TimePassing dt) state = updateDynamic dt state
updateGameState _ state = state

getStationByCoord :: Point -> [Station] -> Maybe Station
getStationByCoord p stations = myStations
  where
    myStations :: Maybe Station
    myStations = find (\a -> withinErrorPosition (getStationPosition a) p 1) stations

handleClick :: Point -> GameState -> GameState
handleClick point state@(GameState stations routes locos assets Play time) = turnConstructionOn
  where
    turnConstructionOn =
      case getStationByCoord point (getStations state) of
        Nothing -> state
        Just x -> GameState stations routes locos assets (Construction green (Just x)) time

handleClick _ state@(GameState _ _ _ _ (Construction _ Nothing) _) = state
handleClick point state@(GameState stations routes locos assets (Construction color (Just startStation)) time) = turnConstructionOff
  where
    turnConstructionOff =
      case getStationByCoord point (getStations state) of
        Nothing -> state
        Just x -> GameState stations newRoutes locos assets Play time
          where
            newRoutes :: [Route]
            -- TODO: remove color hardcode
            newRoutes = 
              case (sameTargetStationRoutes) of
                Nothing -> Route color (getStationPosition startStation) (getStationPosition x) : routes
                Just _ -> Route color (getStationPosition x) (getStationPosition startStation) : routes

            sameTargetStationRoutes :: Maybe Route
            sameTargetStationRoutes = listToMaybe $ (filter (\(Route _ routePos1 _) -> (withinErrorPosition (getStationPosition startStation) routePos1 1)) routes)

handleClick _ (GameState stations routes locos assets mode time) = GameState stations routes locos assets mode time

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
    [Locomotive [] Forward (Ready (3,4) brown)]
    [Asset (LineColor brown) (IsUsed True), Asset Train (IsUsed True)]
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
