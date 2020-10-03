module Types where
import CodeWorld

type Position = Point
data Asset = Bridge | Train | Wagon
data Object = Object Asset Position
type ObjectDrawer = Object -> Picture

data Figure = TriangleFigure | SquareFigure | CircleFigure
data StationType = Triangle | Rectangle | Circle
data Passenger = Passenger StationType

data Station = Station {
                        getStationType               :: StationType,
                        getStationPosition           :: Position,
                        getStationPassengers         :: [Passenger]
                        }


data Direction = Forward | Backward
  deriving (Eq, Show)
data Locomotive = Locomotive {
                              getLocomotiveRoute           :: Route,
                              getLocomotivePassengers      :: [Passenger],
                              getLocomotiveDirection       :: Direction,
                              getLocomotivePosition        :: Double
                              }

data Route = Route Color Position Position


data GameState = GameState {
                            getStations     :: [Station],
                            getRoutes       :: [Route],
                            getLocomotives  :: [Locomotive]
                            }
