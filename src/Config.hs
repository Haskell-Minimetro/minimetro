module Config where
import CodeWorld
import Types

-- | Maximum amount of passengers on the station. Used in considering game over
maxPassangers :: Int
maxPassangers = 30

wagonCapacity :: Int
wagonCapacity = 4

-- maxPassengersOnTrain :: Int
-- maxPassengersOnTrain = 6

-- | What colors can be used for the lines
lineColors :: [Color]
lineColors = [brown, red, blue, green]

-- | Amoutn of assets available at the start of the game
initialAmountOfAssets :: Int
initialAmountOfAssets = 2

-- | Default speed of the locomotive
locomotiveSpeed :: Double
locomotiveSpeed = 0.5

-- | What control we have for the game
controls :: [Control]
controls = 
  [ Control Train (-6, -8), 
  Control Wagon (-4, -8),
  Control (LineColor brown) (-2, -8),
  Control (LineColor red) (0, -8),
  Control (LineColor blue) (2, -8),
  Control (LineColor green) (4, -8) ]