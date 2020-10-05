{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Project where

import ActivityOfEnhancements
import CodeWorld
import Data.Fixed
-- import Data.Text (pack)
import Drawers
import System.Random
import Types
import Data.List (find)
import Config

import Data.Maybe (listToMaybe)

-- | Main function that draw State of the Game
drawGameState :: GameState -> Picture
drawGameState gameState =
  renderObject drawStation (getStations gameState) -- Render all stations
    <> drawControls gameState -- Render all controls
    <> renderObject drawLocomotive (getLocomotives gameState) -- Render all locomotives
    <> renderObject drawRoute (getRoutes gameState) -- Render all routes
    <> backgroundImage

-- | Updates locomotive position given timedelta and the locomotive itself
updateLocomotivePosition :: Double -> Locomotive -> Locomotive
updateLocomotivePosition dt (Locomotive cap passengers direction (OnRoute route progress)) = Locomotive cap passengers direction (OnRoute route newProgress)
  where
    newProgress = calculateProgress direction progress dt locomotiveSpeed
updateLocomotivePosition _dt locomotive = locomotive -- If the locomotive isn't 'OnRoute' we don't update its position

-- | Calculate the progress on the route of the locomotive
calculateProgress ::
  Direction -- Direction of the locomotive
  -> Double -- Current progress of the locomotive
  -> Double -- Time delta
  -> Double -- Speed of the locomotive
  -> Double -- New progress towards the station
calculateProgress Forward progress dt speed = progress + dt * speed
calculateProgress Backward progress dt speed = progress - dt * speed

-- | Inverses the direction
inverseDirection :: Direction -> Direction
inverseDirection Backward = Forward
inverseDirection Forward = Backward

-- | Checks if the given position is within some error wrt another position
withinErrorPosition ::
  Position -- First position
  -> Position  -- Second position
  -> Double -- Epsilon, within which error do we consider
  -> Bool
withinErrorPosition (x, y) (x2, y2) epsilon = withinError x x2 epsilon && withinError y y2 epsilon

-- | Check if given value is within some error wrt another value
withinError :: Double -> Double -> Double -> Bool
withinError a b epsilon = abs (a - b) < epsilon

-- | Updates the function so it only runs if some amount of time has passed
withTimePassing ::
  forall world.
  Double -- Current Time
  -> Double -- Amount of seconds that needs to pass 
  -> (Double -> world -> world) -- The function that will be called every 'x' seconds
  -> (Double -> world -> world) -- Updated function
withTimePassing currentTime threshold func
  | currentTime `mod'` threshold < 0.05 = func
  | otherwise = const id

-- | Retrieves Nth element of the list (if able)
nth :: Int -> [a] -> Maybe a
nth _ []       = Nothing
nth 0 (x : _)  = Just x
nth n (_ : xs) = nth (n - 1) xs

-- TODO: add exponential grow of appearance of new passengers
-- | Update station (add new passenger) given current time delta
updateStation :: Double -> Station -> Station
updateStation _dt (Station stationType pos passengers gen) = Station stationType pos newPassengers newGen
  where
    (number, newGen) = randomR (0 :: Int, 1) gen -- generate new passenger
    possibleValues = filter (\(Passenger x)-> x /= stationType )  [Passenger Triangle, Passenger Rectangle, Passenger Circle] -- Get possible values for new passenger
    newPassenger =
      case nth number possibleValues of -- Get new passenger by 'id'
        Just passenger -> passenger
        Nothing -> Passenger Circle
    newPassengers = passengers ++ [newPassenger] -- append it to the list

-- | Check that transfer from locomotive to station is possible
checkTransfer :: Locomotive -> Station -> Bool
checkTransfer locomotive station =
  case getLocomotiveStatus locomotive of
    (Ready _ _) -> False
    (OnRoute _ _) -> False
    (TransferTo pos _) -> pos == getStationPosition station
    (TransferFrom pos _) -> pos == getStationPosition station

-- TODO: the state of the station may change for transfers, but not for now
-- | Transfer passengers from locomtoive to station (if they are in the same position)
transferPassangersToStation :: Locomotive -> Station -> (Locomotive, Station)
transferPassangersToStation locomotive@(Locomotive cap passengers direction (TransferTo position color)) station
  | checkTransfer locomotive station = (updatedLocomotive, updatedStation) -- Check same position
  | otherwise = (locomotive, station)
  where
    trainPassangers = map Passenger (filter (/=stationType) (map (\(Passenger x) -> x) passengers)) -- Remove all passengers that are the same symbol as the station
    stationType = getStationType station

    updatedLocomotive = Locomotive cap trainPassangers direction (TransferFrom position color)
    updatedStation = station
transferPassangersToStation locomotive station = (locomotive, station)

-- | Transfer passengers from station to locomotive (if they are in the same position)
transferPassangersToLocomotive :: Locomotive -> Station -> (Locomotive, Station)
transferPassangersToLocomotive locomotive@(Locomotive capacity trainPassangers direction (TransferFrom position color)) station
  | checkTransfer locomotive station = (updatedLocomotive, updatedStation) -- Check same position
  | otherwise = (locomotive, station)
  where
    stationPassengers = getStationPassengers station
    maxToTransfer = capacity - length trainPassangers

    newTrainPassangers = trainPassangers ++ take maxToTransfer stationPassengers
    newStationsPassengers = drop maxToTransfer stationPassengers

    updatedLocomotive = Locomotive capacity newTrainPassangers direction (Ready position color)
    updatedStation = Station (getStationType station) (getStationPosition station) newStationsPassengers (getPassengerGen station)
transferPassangersToLocomotive locomotive station = (locomotive, station)

-- | Given list of possible routes and locomotive that is ready to move out from the station
-- Tries to suggest to which route it should move
getTransferRoute :: [Route] -> Locomotive -> Maybe (Route, Direction)
getTransferRoute routes (Locomotive _ _ direction (Ready pos color)) =
  case direction of 
    Forward -> -- Try to follow the same direction
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
    -- Tries to find a route that goes forward from your position
    getTransferForward :: Maybe Route
    getTransferForward = find (\(Route routeColor pos1 _pos2) -> color == routeColor && pos1 == pos) routes

    -- Tries to find a route that goes backward from your position
    getTransferBackward ::Maybe Route
    getTransferBackward = find (\(Route routeColor _pos1 pos2) -> color == routeColor && pos2 == pos) routes
getTransferRoute _ _ = Nothing

-- | Given a list of possible routes and locomotive, moves the locomotive to this position
transferLocomotive :: [Route] -> Locomotive -> Locomotive
transferLocomotive routes locomotive@(Locomotive cap passagners _ (Ready _ _)) = newLocomotive
  where
    whereToStart Backward = 1.0
    whereToStart Forward = 0.0
    newLocomotive = 
      case getTransferRoute routes locomotive of
        Just (newRoute, newDirection) -> Locomotive cap passagners newDirection (OnRoute newRoute (whereToStart newDirection))
        Nothing -> locomotive
transferLocomotive _routes locomotive = locomotive

-- | Get the route position by the direction in which we want to move
directionRouteToPos :: Route -> Direction -> Position
directionRouteToPos (Route _ _ pos2) Forward = pos2
directionRouteToPos (Route _ pos1 _) Backward = pos1


-- | Given the list of stations and locomotive, tries to see if locomotive have reached any of them
stopLocomotive :: [Station] -> Locomotive -> Locomotive
stopLocomotive stations locomotive@(Locomotive cap passengers direction (OnRoute route@(Route color _ _) progress))
  | progress > 1 =  -- If we reach one of the ends of the routes -> transfer to the station
    case getStationByCoord (directionRouteToPos route direction) stations of
      Just station -> Locomotive cap passengers direction (TransferTo (getStationPosition station) color)
      Nothing -> locomotive
  | progress < 0 = 
    case getStationByCoord (directionRouteToPos route direction) stations of
      Just station -> Locomotive cap passengers direction (TransferTo (getStationPosition station) color)
      Nothing -> locomotive
  | otherwise = locomotive
stopLocomotive _stations locomotive = locomotive

-- | Update global clock of game state
updateTime ::
  Double -- Current time
  -> Double -- time delta
  -> GameMode -- Current game mode
  -> Double -- new Time
updateTime currentTime dt Play = currentTime + dt
updateTime currentTime _ _ = currentTime

-- | Helper function that helps moving passengers from and to stations
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

-- | Given list of locomotives and stations tries to exchange passengers where it is possible
transferPassengers :: [Locomotive] -> [Station] -> ([Locomotive], [Station])
transferPassengers trains [] = (trains, [])
transferPassengers trains (first:rest) = (updatedTrains, updatesStations)
  where
    (newTrains, newStation) = transferPassengersHelper trains first

    (nextTrains, nextStations) = transferPassengers newTrains rest
    updatedTrains = nextTrains
    updatesStations = newStation : nextStations

-- | Function that handles dynamic (with time delta) state of the game
updateDynamic :: Double -> GameState -> GameState
updateDynamic dt (GameState stations routes locomotives mode currentTime) = newState
  where
    updatedLocomotives = map (stopLocomotive stations . updateLocomotivePosition dt . transferLocomotive routes) locomotives
    newTime = updateTime currentTime dt mode

    updatedStations = map (withTimePassing currentTime 2 updateStation dt) stations -- New passengers appear every 2 seconds

    (transferredLocomotives, transferredStations) = transferPassengers updatedLocomotives updatedStations
    newState = GameState transferredStations routes transferredLocomotives mode newTime

-- | Update game state by some event
updateGameState :: Event -> GameState -> GameState
updateGameState (PointerPress p) state = handleClick p state 
updateGameState (TimePassing dt) state = updateDynamic dt state
updateGameState _ state = state

-- | Given the point and list of stations, tries to find required station
getStationByCoord :: Point -> [Station] -> Maybe Station
getStationByCoord point stations = myStations
  where
    myStations :: Maybe Station
    myStations = find (\a -> withinErrorPosition (getStationPosition a) point 1) stations

-- | Get Control button by given some point
getControlByCoord :: Point -> Maybe Control
getControlByCoord point =
  case foundControl of
    Just control -> Just control -- If normal control was located - give it
    Nothing -> case find (\(Control _ (x, y)) -> withinErrorPosition (x+1, y+1) point 0.5) controls of -- Otherwise, try to locate the 'Removal' of the corresponding control
      Just (Control assetType _) -> Just (Removal assetType)
      Just x -> Just x
      Nothing -> Nothing
  where
    foundControl :: Maybe Control
    foundControl = find (\(Control _ pos) -> withinErrorPosition pos point 0.5) controls

-- | Given locomotive, retrieves it's color
getLocomotiveColor :: Locomotive -> Color
getLocomotiveColor (Locomotive _ _ _ (Ready _ trainColor)) = trainColor
getLocomotiveColor (Locomotive _ _ _ (TransferTo _ trainColor)) = trainColor
getLocomotiveColor (Locomotive _ _ _ (TransferFrom _ trainColor)) = trainColor
getLocomotiveColor (Locomotive _ _ _ (OnRoute (Route trainColor _ _) _)) = trainColor

-- | Removes all of AssetType from the game state
removeAssetType :: AssetType -> GameState -> GameState
removeAssetType (LineColor routeColor) (GameState stations routes locos mode time) = newState
  where
    newState = GameState stations newRoutes newLocos mode time
    newRoutes = filter (\(Route color _ _) -> color /= routeColor) routes
    newLocos = filter (\locomotive -> getLocomotiveColor locomotive /= routeColor) locos

removeAssetType Train (GameState stations routes _ mode time) = GameState stations routes [] mode time
removeAssetType _ state = state

-- | Function that handles clicking
-- Can transfer game state from Play to Construction, Repopulation and others
handleClick :: Point -> GameState -> GameState
handleClick point state@(GameState stations routes locos Play time) 
  = withColorPicked
    where
      outOfAssets = getAmountOfAssets time <= getAmountOfAssetsUsed state

      colors = getUniqueLinesColors routes
      isAvailable asset = isAssetAvailable outOfAssets asset colors 

      withColorPicked =
        case getControlByCoord point of
          Nothing -> state
          Just (Control Train _) ->
            if isAvailable Train then GameState stations routes locos Repopulation time else state
          Just (Control Wagon _) -> 
            if isAvailable Wagon then GameState stations routes locos WagonAddition time else state
          Just (Control (LineColor color) _) ->
            if isAvailable (LineColor color) then GameState stations routes locos (Construction color Nothing) time else state
          Just (Removal assetType) -> removeAssetType assetType state

handleClick point state@(GameState stations routes locos Repopulation time)
  = case getControlByCoord point of 
      Nothing -> state
      Just (Removal _) -> state
      Just (Control Train _) -> state
      Just (Control Wagon _) -> state
      Just (Control (LineColor trainColor) _ ) -> case chosenRoute of 
          Nothing -> GameState stations routes locos Play time
          Just (Route _ startPosition _) -> GameState stations routes (Locomotive wagonCapacity [] Forward (Ready startPosition trainColor):locos) Play time
        where
          chosenRoute = find (\(Route color _ _ ) -> color == trainColor) routes

handleClick _ (GameState stations routes [] WagonAddition time) = GameState stations routes [] Play time

handleClick point state@(GameState stations routes locos WagonAddition time)
  = case getControlByCoord point of 
      Nothing -> state
      Just (Removal _) -> state
      Just (Control Train _) -> state
      Just (Control Wagon _) -> state
      Just (Control (LineColor trainColor) _ ) -> case chosenRoute of 
          Nothing -> GameState stations routes locos Play time
          Just Route {} -> case listToMaybe leftLocos of 
            Nothing -> GameState stations routes locos Play time
            Just loco -> GameState stations routes (drop 1 leftLocos ++ rightLocos ++ [newLoco loco] ) Play time
        where
          chosenRoute = find (\(Route color _ _ ) -> color == trainColor) routes

          leftLocos :: [Locomotive]
          leftLocos = dropWhile ((/=trainColor) . getLocomotiveColor) locos
          
          rightLocos :: [Locomotive]
          rightLocos = takeWhile ((/=trainColor) . getLocomotiveColor) locos

          newLoco l = Locomotive ((getLocomotiveCapacity l) + wagonCapacity) (getLocomotivePassengers l) (getLocomotiveDirection l) (getLocomotiveStatus l)

handleClick point state@(GameState stations routes locos (Construction color Nothing) time) = turnConstructionOn
  where
    turnConstructionOn =
      case getStationByCoord point (getStations state) of
        Nothing -> state
        Just x -> GameState stations routes locos (Construction color (Just x)) time

handleClick point state@(GameState stations routes locos (Construction color (Just startStation)) time) = turnConstructionOff
  where
    turnConstructionOff =
      case getStationByCoord point (getStations state) of
        Nothing -> state
        Just secondStation -> GameState stations newRoutes locos Play time
          where
            newRoutes = 
              case createNewRoute routes color startStation secondStation of
                Nothing -> routes
                Just route -> route : routes

-- | Tries to suggest what route should be created
createNewRoute ::
  [Route] -- Current Routes
  -> Color -- Color of new route
  -> Station -- Starting station by the user
  -> Station -- Second station by the user
  -> Maybe Route -- Suggest a new Route
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

      -- Others cases are hard to describe. The basic idea that every node should have at most Input route and one Output route
      | sumFirstPos == 0 && routesInSecondPos == 1 = Just (Route routeColor secondPos firstPos)
      | sumFirstPos == 0 && routesOutSecondPos == 1 = Just (Route routeColor firstPos secondPos)

      | routesInFirstPos == 1 && (sumSecondPos == 0 || routesOutSecondPos == 1) = Just (Route routeColor firstPos secondPos)
      | routesInFirstPos == 1 && routesInSecondPos == 1 = Nothing

      | routesOutFirstPos == 1 && (sumSecondPos == 0 || routesInSecondPos == 1) = Just (Route routeColor secondPos firstPos)
      | routesOutFirstPos == 1 && routesOutSecondPos == 1 = Nothing
      
      | otherwise = Nothing

-- | Intial state of the game
initialSystem :: GameState
initialSystem =
  GameState
    [ Station Circle (3, 4) [] (mkStdGen 43),
      Station Rectangle (2, -6) [] (mkStdGen 41),
      Station Triangle (-6, 2) [] (mkStdGen 40),
      Station Triangle (4, 2) [] (mkStdGen 39),
      Station Rectangle (0, 8) [] (mkStdGen 38),
      Station Circle (-4, -4) [] (mkStdGen 37)
    ] -- Some stations
    [] -- No Route
    [] -- No Locomotives
    Play
    0 -- 0 at the clock

-- | Check if game is over (if at least one station has more than maxPassangers on it)
isGameOver :: GameState -> Bool
isGameOver state = stationsAreFull (getStations state)
  where
    stationsAreFull :: [Station] -> Bool
    stationsAreFull [] = False
    stationsAreFull (first : rest) = length (getStationPassengers first) >= maxPassangers || stationsAreFull rest

-- TODO: Maybe some better showage that its over
-- | Updates function activity of so that it check if game is over and ignores input afterwards
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
      | isOver state = translated 9.6 9.2 (colored red (lettering "Game Over")) <> userDrawer state
      | otherwise = userDrawer state

-- | Create resettable game with starting screen that checks for game over
run :: IO ()
run = withGameOver isGameOver (withStartScreen (withReset activityOf)) initialSystem updateGameState drawGameState
