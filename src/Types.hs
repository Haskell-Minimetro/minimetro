module Types where
import CodeWorld

type Position = Point
data Asset = Bridge | Train | Wagon
data Object = Object Asset Position
type ObjectDrawer = Object -> Picture


data StationType = Triangle | Rectangle | Circle
data Passenger = Passenger StationType
data Station = Station StationType Position [Passenger]


data Route = Route Color 


data GameState = GameState 