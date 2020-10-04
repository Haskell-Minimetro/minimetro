module Types where
import CodeWorld
import System.Random

type Position = Point

data AssetType = LineColor Color | Train | Wagon 
  deriving (Eq, Show)

newtype IsUsed = IsUsed Bool
data Asset = Asset AssetType IsUsed

data Control = Control AssetType Position

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

data StationStatus = OnRoute Route Double | TransferTo Position Color | TransferFrom Position Color | Ready Position Color
  deriving (Show)
  
data Locomotive = Locomotive {
                              getLocomotivePassengers      :: [Passenger],
                              getLocomotiveDirection       :: Direction,
                              getLocomotiveStatus          :: StationStatus
                              }

data Route = Route Color Position Position
  deriving (Show)

data GameMode = Play | Construction Color (Maybe Station) | GameOver
  deriving (Show)


data GameState = GameState {
                            getStations     :: [Station],
                            getRoutes       :: [Route],
                            getLocomotives  :: [Locomotive],
                            getAssets       :: [Asset],
                            getCurrentMode  :: GameMode,
                            getCurrentTime  :: Double
                            }
