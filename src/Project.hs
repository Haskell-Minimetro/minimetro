{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Project where

import qualified CodeWorld              as CW
import qualified System.Random          as Random
import qualified Data.Fixed             as Fixed
import qualified Data.List              as List   (find)
import qualified Data.Maybe             as Maybe  (listToMaybe)

import qualified ActivityOfEnhancements as AOE
import qualified Drawers
import Types
import qualified Config

-- | Update locomotive position if it is on Route
updateLocomotivePosition :: Double -> Locomotive -> Locomotive
updateLocomotivePosition dt (Locomotive passengers direction (OnRoute route progress)) 
  = Locomotive passengers direction (OnRoute route newProgress)
    where
      newProgress = calculateProgress direction progress dt Config.locomotiveSpeed
updateLocomotivePosition _dt locomotive = locomotive

-- | Calculate new progress of a locomotive being on route
calculateProgress 
  :: Direction -- locomotive's direction on route
  -> Double    -- previous progress
  -> Double    -- time passed
  -> Double    -- locomotive's speed 
  -> Double    -- new progress
calculateProgress Forward progress dt speed = progress + dt * speed
calculateProgress Backward progress dt speed = progress - dt * speed

-- | Inverts direction
inverseDirection :: Direction -> Direction
inverseDirection Backward = Forward
inverseDirection Forward = Backward

-- | Estimates if 2 points are in the same neighbourhood (epsilon * epsilon)
withinErrorPosition :: Position -> Position -> Double -> Bool
withinErrorPosition (x1, y1) (x2, y2) epsilon 
  =  withinError x1 x2 epsilon 
  && withinError y1 y2 epsilon

-- | Estimates if 2 Numbers are in the same neighbourhood
withinError :: (Ord a, Num a) => a -> a -> a -> Bool
withinError a b epsilon = abs (a - b) < epsilon

withTimePassing :: forall world. Double -> Double -> (Double -> world -> world) -> (Double -> world -> world)
withTimePassing currentTime threshold func
  | currentTime `Fixed.mod'` threshold < 0.05 = func
  | otherwise = const id


nth :: Int -> [a] -> Maybe a
nth _ []       = Nothing
nth 0 (x : _)  = Just x
nth n (_ : xs) = nth (n - 1) xs

-- TODO: add exponential grow of appearance of new passengers
updateStation :: Double -> Station -> Station
updateStation _dt (Station stationType pos passengers gen) = Station stationType pos newPassengers newGen
  where
    (number, newGen) = Random.randomR (0 :: Int, 1) gen
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


routeFilter :: CW.Color -> Position -> Route -> Bool
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
    getTransferForward = List.find (\(Route routeColor pos1 _pos2) -> color == routeColor && pos1 == pos) routes

    getTransferBackward ::Maybe Route
    getTransferBackward = List.find (\(Route routeColor _pos1 pos2) -> color == routeColor && pos2 == pos) routes
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

-- TODO: passanger setting on the train
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
    updatedLocomotives = map (stopLocomotive stations . updateLocomotivePosition dt . transferLocomotive routes) locomotives -- Some filter on routes/locomotives
    -- TODO: transferPassangersFromStation . transferPassangersToStation 
    newTime = updateTime currentTime dt mode

    updatedStations = map (withTimePassing currentTime 2 updateStation dt) stations

    (transferredLocomotives, transferredStations) = transferPassengers updatedLocomotives updatedStations
    newState = GameState transferredStations routes transferredLocomotives assets mode newTime


-- TODO: Creation of routes by point click
-- TODO: addition of locomotive by point and click
updateGameState :: CW.Event -> GameState -> GameState
updateGameState (CW.PointerPress p) state = handleClick p state
updateGameState (CW.TimePassing dt) state = updateDynamic dt state
updateGameState _ state = state

getStationByCoord :: CW.Point -> [Station] -> Maybe Station
getStationByCoord point stations = myStations
  where
    myStations :: Maybe Station
    myStations = List.find (\a -> withinErrorPosition (getStationPosition a) point 1) stations

getControlByCoord :: CW.Point -> Maybe Control
getControlByCoord point = foundControl
  where
    foundControl :: Maybe Control
    foundControl = List.find (\(Control _ pos) -> withinErrorPosition pos point 0.5) Config.controls

handleClick :: CW.Point -> GameState -> GameState
handleClick point state@(GameState stations routes locos assets Play time) = withColorPicked
  where
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
        Just x -> GameState stations newRoutes locos assets Play time
          where
            newRoutes :: [Route]
            -- TODO: remove color hardcode
            newRoutes = 
              case (sameTargetStationRoutes) of
                Nothing -> Route color (getStationPosition startStation) (getStationPosition x) : routes
                Just _ -> Route color (getStationPosition x) (getStationPosition startStation) : routes

            sameTargetStationRoutes :: Maybe Route
            sameTargetStationRoutes = Maybe.listToMaybe $ (filter (\(Route _ routePos1 _) -> (withinErrorPosition (getStationPosition startStation) routePos1 1)) routes)

handleClick _ (GameState stations routes locos assets mode time) = GameState stations routes locos assets mode time

initialSystem :: GameState
initialSystem =
  GameState
    [ Station Circle (3, 4) [] (Random.mkStdGen 42),
      --  [Passenger Circle, Passenger Rectangle, Passenger Triangle, Passenger Circle, Passenger Rectangle, Passenger Triangle, Passenger Circle, Passenger Rectangle, Passenger Triangle, Passenger Circle,
      --  Passenger Rectangle, Passenger Triangle, Passenger Circle, Passenger Rectangle, Passenger Triangle ,Passenger Circle, Passenger Rectangle, Passenger Triangle ,Passenger Circle, Passenger Rectangle,
      --  Passenger Triangle ,Passenger Circle, Passenger Rectangle, Passenger Triangle ,Passenger Circle, Passenger Rectangle, Passenger Triangle, Passenger Circle, Passenger Rectangle, Passenger Triangle],
      Station Rectangle (2, -6) [] (Random.mkStdGen 41),
      Station Triangle (-4, 2) [] (Random.mkStdGen 40),
      Station Triangle (4, 2) [] (Random.mkStdGen 40)
    ]
    [Route CW.brown (3, 4) (2, -6), Route CW.brown (2, -6) (-4, 2)]
    [Locomotive [] Forward (Ready (3,4) CW.brown)]
    [Asset (LineColor CW.brown) (IsUsed True), Asset Train (IsUsed True)]
    Play
    0

isGameOver :: GameState -> Bool
isGameOver state = stationsAreFull (getStations state)
  where
    stationsAreFull :: [Station] -> Bool
    stationsAreFull [] = False
    stationsAreFull (first : rest) = length (getStationPassengers first) >= Config.maxPassangers || stationsAreFull rest

-- TODO: Maybe some better showage that its over
withGameOver 
  :: forall world
  . (world -> Bool) 
  -> AOE.ActivityOf world 
  -> AOE.ActivityOf world
withGameOver isOver originalActivityOf userWorld userHandler userDrawer =
  originalActivityOf userWorld ignoreInput updatedDrawer
  where
    ignoreInput :: CW.Event -> world -> world
    ignoreInput event state
      | isOver state = state
      | otherwise = userHandler event state

    updatedDrawer :: world -> CW.Picture
    updatedDrawer state
      | isOver state = CW.translated 9 9 (CW.lettering "Game Over") <> userDrawer state
      | otherwise = userDrawer state

run2 :: IO ()
run2 = withGameOver isGameOver (AOE.withStartScreen (AOE.withReset CW.activityOf)) initialSystem updateGameState Drawers.drawGameState

run :: IO ()
run = CW.debugActivityOf initialSystem updateGameState Drawers.drawGameState
