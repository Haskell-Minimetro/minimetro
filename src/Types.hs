module Types where
import CodeWorld
import System.Random

type Position = Point

data AssetType = LineColor Color | Train | Wagon 
  deriving (Eq, Show)

data Control = Control AssetType Position | Removal AssetType

data Figure = TriangleFigure | SquareFigure | CircleFigure
data StationType = Triangle | Rectangle | Circle
  deriving (Eq, Show)

newtype Passenger = Passenger StationType
  deriving (Eq, Show)

data Station = Station {
                        getStationType               :: StationType,
                        getStationPosition           :: Position,
                        getStationPassengers         :: [Passenger],
                        getPassengerGen              :: StdGen 
                        }
  deriving (Show)


data Direction = Forward | Backward
  deriving (Eq, Show)

-- TODO: There is no need for position now?
data LocomotiveStatus = OnRoute Route Double | TransferTo Position Route | TransferFrom Position Route | Ready Position Route
  deriving (Show)
  
data Locomotive = Locomotive {
                              getLocomotivePassengers      :: [Passenger],
                              getLocomotiveDirection       :: Direction,
                              getLocomotiveStatus          :: LocomotiveStatus
                              }

data Route = Route {
                    getRouteColor          :: Color,
                    getFirstRoutePosition  :: Position,
                    getSecondRoutePosition :: Position
                   }
  deriving (Eq, Show)

data GameMode = Play | Repopulation | Construction Color (Maybe Station) 
  deriving (Show)


data GameState = GameState {
                            getStations     :: [Station],
                            getRoutes       :: [Route],
                            getLocomotives  :: [Locomotive],
                            getCurrentMode  :: GameMode,
                            getCurrentTime  :: Double
                            }
