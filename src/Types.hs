module Types where

import qualified CodeWorld as CW
import qualified System.Random as Random

-- | Position of an object on map
type Position = CW.Point

-- | Asset Type : Line with Color, Train or Wagon
data AssetType = LineColor CW.Color | Train | Wagon 
  deriving (Eq, Show)

-- | IsUsed wrapper
newtype IsUsed = IsUsed Bool

-- | Asset holds: Asset Type and Usage status
data Asset = Asset AssetType IsUsed

-- | Control holds: Asset Type and Position
data Control = Control AssetType Position

-- | Drawable Figure
data Figure = TriangleFigure | SquareFigure | CircleFigure

-- | Station Type : Triangle, Rectangle or Circle
data StationType = Triangle | Rectangle | Circle
  deriving (Eq, Show)

-- | Passengers are defined by their destination Station Type
newtype Passenger = Passenger StationType
  deriving (Eq, Show)

-- | Station holds: Type, Position, Passengers List and Passenger generator
data Station = Station {
                        getStationType               :: StationType,
                        getStationPosition           :: Position,
                        getStationPassengers         :: [Passenger],
                        getPassengerGen              :: Random.StdGen 
                        }
  deriving (Show)

-- | Direction
data Direction = Forward | Backward
  deriving (Eq, Show)

-- | 
data StationStatus = OnRoute Route Double | TransferTo Position CW.Color | TransferFrom Position CW.Color | Ready Position CW.Color
  deriving (Show)

-- | Locomotive 
data Locomotive = Locomotive {
                              getLocomotivePassengers      :: [Passenger],
                              getLocomotiveDirection       :: Direction,
                              getLocomotiveStatus          :: StationStatus
                              }

-- | Route with its color, start and stop station positions
data Route = Route {
                    getRouteColor   :: CW.Color, 
                    routeStart      :: Position, 
                    routeStop       :: Position
                    } deriving (Show)

-- | GameMode
-- | Play - no construction, just play
-- | Construction - construction of a new Route or Station        
-- | GameOver - end of the game            
data GameMode = Play | Construction CW.Color (Maybe Station) | GameOver
  deriving (Show)

-- | GameState holds everything we need for a game:
-- | available stations, routes, locomotives, 
-- | assets (line color, train, wagon), game mode and current time
data GameState = GameState {
                            getStations     :: [Station],
                            getRoutes       :: [Route],
                            getLocomotives  :: [Locomotive],
                            getAssets       :: [Asset],
                            getCurrentMode  :: GameMode,
                            getCurrentTime  :: Double
                            }
