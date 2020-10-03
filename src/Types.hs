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
data Locomotive = Locomotive {
                              getLocomotiveRoute           :: Route,
                              getLocomotivePassengers      :: [Passenger],
                              getLocomotiveDirection       :: Direction,
                              getLocomotivePosition        :: Double
                              }

data Route = Route Color Position Position
  deriving (Show)
data GameMode = Play | Construction Station | GameOver
  deriving (Show)

data GameState = GameState {
                            getStations     :: [Station],
                            getRoutes       :: [Route],
                            getLocomotives  :: [Locomotive],
                            getCurrentMode  :: GameMode,
                            getCurrentTime  :: Double
                            }
