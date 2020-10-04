module Config where
import CodeWorld
import Types

maxPassangers :: Int
maxPassangers = 30

maxPassengersOnTrain :: Int
maxPassengersOnTrain = 6

lineColors :: [Color]
lineColors = [brown, red, blue, green]

initialAmountOfAssets :: Int
initialAmountOfAssets = 2

controls :: [Control]
controls = 
  [ Control Train (-6, -8), 
  Control Wagon (-4, -8),
  Control (LineColor brown) (-2, -8),
  Control (LineColor red) (0, -8),
  Control (LineColor blue) (2, -8),
  Control (LineColor green) (4, -8) ]