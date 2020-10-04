module Types where
import CodeWorld
import System.Random

type Position = Point
data Asset = Bridge | Train | Wagon
  deriving (Eq, Show)
data Object = Object Asset Position
  deriving (Eq, Show)
type ObjectDrawer = Object -> Picture

data Figure = TriangleFigure | SquareFigure | CircleFigure
data StationType = Triangle | Rectangle | Circle
  deriving (Eq, Show)

data Passenger = Passenger StationType
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

data Route = Route {
  getColor :: Color,
  from :: Position,
  to :: Position
} deriving (Eq, Show)


data GameMode = Play | Construction Station | GameOver
  deriving (Show)

data GameState = GameState {
                            getStations     :: [Station],
                            getRoutes       :: [Route],
                            getLocomotives  :: [Locomotive],
                            getCurrentMode  :: GameMode,
                            getCurrentTime  :: Double
                            }
