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
import Config

drawGameState :: GameState -> Picture
drawGameState gameState =
  renderObject drawStation (getStations gameState)
    <> translated (-9) (-6) (lettering (pack $ show (getCurrentMode gameState)))
    <> translated (-9) (-4) (lettering (pack $ show (length (getRoutes gameState))))
    <> drawControls gameState
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

withinErrorPosition :: Position -> Position -> Double -> Bool
withinErrorPosition (x, y) (x2, y2) epsilon = withinError x x2 epsilon && withinError y y2 epsilon

withinError :: Double -> Double -> Double -> Bool
withinError a b epsilon = abs (a - b) < epsilon

withTimePassing :: forall world. Double -> Double -> (Double -> world -> world) -> (Double -> world -> world)
withTimePassing currentTime threshold func
  | currentTime `mod'` threshold < 0.05 = func
  | otherwise = const id


nth :: Int -> [a] -> Maybe a
nth _ []       = Nothing
nth 0 (x : _)  = Just x
nth n (_ : xs) = nth (n - 1) xs

-- TODO: add exponential grow of appearance of new passengers
updateStation :: Double -> Station -> Station
updateStation _dt (Station stationType pos passengers gen) = Station stationType pos newPassengers newGen
  where
    (number, newGen) = randomR (0 :: Int, 1) gen
    newPassenger =
      case nth number (filter (\(Passenger x)-> x /= stationType ) possibleValues) of
        Just passenger -> passenger
        Nothing -> Passenger Circle
    possibleValues = [Passenger Triangle, Passenger Rectangle, Passenger Circle]
    newPassengers = passengers ++ [newPassenger]

getRandomPassenger :: Int -> Passenger
getRandomPassenger 0 = Passenger Triangle
getRandomPassenger 1 = Passenger Rectangle
getRandomPassenger _ = Passenger Circle


checkTransfer :: Locomotive -> Station -> Bool
checkTransfer locomotive station =
  case getLocomotiveStatus locomotive of
    (Ready _ _) -> False
    (OnRoute _ _) -> False
    (TransferTo pos _) -> pos == getStationPosition station
    (TransferFrom pos _) -> pos == getStationPosition station

-- TODO: the state of the station may change for transfers, but not for now
transferPassangersToStation :: Locomotive -> Station -> (Locomotive, Station)
transferPassangersToStation locomotive@(Locomotive passengers direction (TransferTo position color)) station
  | checkTransfer locomotive station = (updatedLocomotive, updatedStation)
  | otherwise = (locomotive, station)
  where
    trainPassangers = map Passenger (filter (/=stationType) (map (\(Passenger x) -> x) passengers))
    stationType = getStationType station

    updatedLocomotive = Locomotive trainPassangers direction (TransferFrom position color)
    updatedStation = station
transferPassangersToStation locomotive station = (locomotive, station)

transferPassangersToLocomotive :: Locomotive -> Station -> (Locomotive, Station)
transferPassangersToLocomotive locomotive@(Locomotive trainPassangers direction (TransferFrom position color)) station
  | checkTransfer locomotive station = (updatedLocomotive, updatedStation)
  | otherwise = (locomotive, station)
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
        Just routeBackward -> Just (routeBackward, Backward)
        Nothing -> case getTransferForward of
                    Just routeForward -> Just (routeForward, Forward)
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
      Just station -> Locomotive passengers direction (TransferTo (getStationPosition station) color)
      Nothing -> locomotive
  | progress < 0 = 
    case getStationByCoord (directionRouteToPos route direction) stations of
      Just station -> Locomotive passengers direction (TransferTo (getStationPosition station) color)
      Nothing -> locomotive
  | otherwise = locomotive
stopLocomotive _stations locomotive = locomotive

updateTime :: Double -> Double -> GameMode -> Double
updateTime currentTime dt Play = currentTime + dt
updateTime currentTime _ _ = currentTime

transferPassengersHelper :: [Locomotive] -> Station -> ([Locomotive], Station)
transferPassengersHelper [] station = ([], station)
transferPassengersHelper (locomotive:rest) station = (newLocomotive:nextLocomotives, nextStation)
  where
    (newLocomotive, newStation) =
      case getLocomotiveStatus locomotive of
        (OnRoute _ _) -> (locomotive, station)
        (TransferTo _ _) -> transferPassangersToStation locomotive station
        (TransferFrom _ _) -> transferPassangersToLocomotive locomotive station
        (Ready _ _) -> (locomotive, station)
    (nextLocomotives, nextStation) = transferPassengersHelper rest newStation

transferPassengers :: [Locomotive] -> [Station] -> ([Locomotive], [Station])
transferPassengers trains [] = (trains, [])
transferPassengers trains (first:rest) = (updatedTrains, updatesStations)
  where
    (newTrains, newStation) = transferPassengersHelper trains first

    (nextTrains, nextStations) = transferPassengers newTrains rest
    updatedTrains = nextTrains
    updatesStations = newStation : nextStations

updateDynamic :: Double -> GameState -> GameState
updateDynamic dt (GameState stations routes locomotives assets mode currentTime) = newState
  where
    updatedLocomotives = map (stopLocomotive stations . updateLocomotivePosition dt . transferLocomotive routes) locomotives
    newTime = updateTime currentTime dt mode

    updatedStations = map (withTimePassing currentTime 2 updateStation dt) stations

    (transferredLocomotives, transferredStations) = transferPassengers updatedLocomotives updatedStations
    newState = GameState transferredStations routes transferredLocomotives assets mode newTime


-- TODO: addition of locomotive by point and click
updateGameState :: Event -> GameState -> GameState
updateGameState (PointerPress p) state = handleClick p state
updateGameState (TimePassing dt) state = updateDynamic dt state
updateGameState _ state = state

getStationByCoord :: Point -> [Station] -> Maybe Station
getStationByCoord point stations = myStations
  where
    myStations :: Maybe Station
    myStations = find (\a -> withinErrorPosition (getStationPosition a) point 1) stations

getControlByCoord :: Point -> Maybe Control
getControlByCoord point = foundControl
  where
    foundControl :: Maybe Control
    foundControl = find (\(Control _ pos) -> withinErrorPosition pos point 0.5) controls

handleClick :: Point -> GameState -> GameState
handleClick point state@(GameState stations routes locos assets Play time) 
  = if isEnabled then  withColorPicked else state
    where
      week = floor (time / 10)
      isEnabled = week > length assets - 3
      withColorPicked =
        case getControlByCoord point of
          Nothing -> state
          Just (Control Train _) -> state
          Just (Control Wagon _) -> state
          Just (Control (LineColor color) _) -> GameState stations routes locos assets (Construction color Nothing) time

handleClick point state@(GameState stations routes locos assets (Construction color Nothing) time) = turnConstructionOn
  where
    turnConstructionOn =
      case getStationByCoord point (getStations state) of
        Nothing -> state
        Just x -> GameState stations routes locos assets (Construction color (Just x)) time


handleClick point state@(GameState stations routes locos assets (Construction color (Just startStation)) time) = turnConstructionOff
  where
    turnConstructionOff =
      case getStationByCoord point (getStations state) of
        Nothing -> state
        Just secondStation -> GameState stations newRoutes locos newAssets Play time
          where
            newAssets = Asset (LineColor color) (IsUsed True) : assets
            newRoutes = 
              case createNewRoute routes color startStation secondStation of
                Nothing -> routes
                Just route -> route : routes

handleClick _ state = state

createNewRoute :: [Route] -> Color -> Station -> Station -> Maybe Route
createNewRoute routes routeColor firstStation secondStation = newRoute
  where
    filteredRoutes = filter (\(Route color _ _ ) -> color == routeColor) routes

    firstPos = getStationPosition firstStation
    secondPos = getStationPosition secondStation
    
    routesIn posToCheck = filter (\(Route _ _ pos2) -> pos2 == posToCheck) filteredRoutes
    routesOut posToCheck = filter (\(Route _ pos1 _) -> pos1 == posToCheck) filteredRoutes

    routesInFirstPos = length (routesIn firstPos)
    routesOutFirstPos = length (routesOut firstPos)
    routesInSecondPos = length (routesIn secondPos)
    routesOutSecondPos = length (routesOut secondPos)

    sumFirstPos = routesInFirstPos + routesOutFirstPos
    sumSecondPos = routesInSecondPos + routesOutSecondPos

    newRoute
      | firstPos == secondPos = Nothing -- First case, positions are the same - we can't do that
      | sumFirstPos == 0 && sumSecondPos == 0 = Just (Route routeColor firstPos secondPos) -- Second case, both points have no routes, we just create new one
      | sumFirstPos == 2 || sumSecondPos == 2 = Nothing -- Third case, at least one of the points have in and out, we can't create a new route

      | sumFirstPos == 0 && routesInSecondPos == 1 = Just (Route routeColor secondPos firstPos)
      | sumFirstPos == 0 && routesOutSecondPos == 1 = Just (Route routeColor firstPos secondPos)

      | routesInFirstPos == 1 && (sumSecondPos == 0 || routesOutSecondPos == 1) = Just (Route routeColor firstPos secondPos)
      | routesInFirstPos == 1 && routesInSecondPos == 1 = Nothing

      | routesOutFirstPos == 1 && (sumSecondPos == 0 || routesInSecondPos == 1) = Just (Route routeColor secondPos firstPos)
      | routesOutFirstPos == 1 && routesOutSecondPos == 1 = Nothing
      
      | otherwise = Nothing

initialSystem :: GameState
initialSystem =
  GameState
    [ Station Circle (3, 4) [] (mkStdGen 42),
      Station Rectangle (2, -6) [] (mkStdGen 41),
      Station Triangle (-4, 2) [] (mkStdGen 40),
      Station Triangle (4, 2) [] (mkStdGen 39)
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
