module Types where
import CodeWorld
import System.Random

type Position = Point
data Asset = Bridge | Train | Wagon
data Object = Object Asset Position
type ObjectDrawer = Object -> Picture

data Figure = TriangleFigure | SquareFigure | CircleFigure
data StationType = Triangle | Rectangle | Circle
  deriving (Eq)
data Passenger = Passenger StationType

data Station = Station {
                        getStationType               :: StationType,
                        getStationPosition           :: Position,
                        getStationPassengers         :: [Passenger],
                        getPassengerGen              :: StdGen 
                        }


data Direction = Forward | Backward
  deriving (Eq, Show)

data StationStatus = OnRoute Route Double | TransferTo Position Color | TransferFrom Position Color | Ready Position Color
data Locomotive = Locomotive {
                              getLocomotivePassengers      :: [Passenger],
                              getLocomotiveDirection       :: Direction,
                              getLocomotiveStatus          :: StationStatus
                              }

data Route = Route Color Position Position


data GameState = GameState {
                            getStations     :: [Station],
                            getRoutes       :: [Route],
                            getLocomotives  :: [Locomotive],
                            getCurrentTime  :: Double
                            }
