{-# LANGUAGE OverloadedStrings #-}
module Drawers where 
import Types
import CodeWorld
import Config

backgroundImage :: Picture
backgroundImage = colored (lighter 0.5 brown) (solidRectangle 100 100)


drawStationType :: StationType -> Picture
drawStationType Triangle =
  scaled 0.85 0.85 (colored white (drawFigure TriangleFigure))
  <> colored black (drawFigure TriangleFigure)
drawStationType Rectangle =
  scaled 0.85 0.85 (colored white (drawFigure SquareFigure))
  <> colored black (drawFigure SquareFigure)
drawStationType Circle =
  scaled 0.85 0.85 (colored white (drawFigure CircleFigure))
  <> colored black (drawFigure CircleFigure)

drawPassanger :: Passenger -> Picture
drawPassanger (Passenger Triangle) = scaled 0.2 0.2 (drawFigure TriangleFigure)
drawPassanger (Passenger Rectangle) = scaled 0.2 0.2 (drawFigure SquareFigure)
drawPassanger (Passenger Circle) = scaled 0.2 0.2 (drawFigure CircleFigure)

-- TODO: fix this triangle draw wrong
drawFigure :: Figure -> Picture
drawFigure TriangleFigure = triangle
  where
    triangle = solidPolygon [(-triangleSize/2, -(median-radius)), (0, radius), (triangleSize/2, -(median-radius))]
    triangleSize = 1.2
    radius = triangleSize * sqrt 3/3
    median = sqrt 3 * triangleSize / 2

drawFigure SquareFigure = solidRectangle 1 1
drawFigure CircleFigure = solidCircle 0.5

drawCurrentDate :: Double -> Picture
drawCurrentDate _time = blank



drawStation :: Station -> Picture
drawStation (Station stationType (x, y) passangers _) 
  = translated x y (drawStationType stationType) <> drawPassengersOnStation (x, y) passangers


drawInARow :: [a] -> Double -> (a->Picture) -> Picture
drawInARow [] _ _ = blank
drawInARow (first:rest) margin drawer = 
  drawer first
  <> translated margin 0 (drawInARow rest margin drawer)

drawPassengersOnStation :: Position -> [Passenger] -> Picture
drawPassengersOnStation (x, y) passengers = translated (x+0.5) (y+0.5) (passes passengers)
  where
    passes :: [Passenger] -> Picture
    passes [] = blank
    passes passenger = drawInARow (take 10 passenger) 0.25 drawPassanger <> translated 0 0.25 (passes (drop 10 passenger))


-- | Render a list of given objects by a given function
renderObject :: (a -> Picture) -> [a] -> Picture
renderObject func = pictures . map func


drawRoute :: Route -> Picture
drawRoute (Route color station1 station2) = colored color (thickCurve 0.3 positions) -- <> translated x y (colored red $ solidCircle 0.7)
  where
    positions = [station1, station2]

drawLocomotive :: Locomotive -> Picture
drawLocomotive (Locomotive trainPassangers _direction others) = drawing
  -- <> translated (-9) (-8) (lettering (pack $ show  direction))
  -- <> translated (-9) (-9) (lettering (pack $ show  others))
  where
    drawing =
      case others of 
        OnRoute (Route color pos1 pos2) progress -> baseDrawing (getTrainPositionWithProgress progress pos1 pos2) (-(pi / 2 - getAngle pos1 pos2)) color
        TransferTo pos color -> baseDrawing pos 0 color
        TransferFrom pos color -> baseDrawing pos 0 color
        Ready pos color -> baseDrawing pos 0 color
    baseDrawing (x, y) rotation color =  translated x y (rotated rotation (passengers color <> locomotiveBase color))
    locomotiveBase locomotiveColor = colored (light locomotiveColor) (solidRectangle 0.66 1)
    passengers locomotiveColor = translated (-0.13) (-0.25) (colored (lighter 0.4 locomotiveColor) (drawPassengersOnTrain trainPassangers))
      

getTrainPositionWithProgress :: Double -> Position -> Position -> Position
getTrainPositionWithProgress percent (x1, y1) (x2, y2) = (x1 * (1 - percent) + x2 * percent, y1 * (1 - percent) + y2 * percent)

getAngle :: Position -> Position -> Double
getAngle (x1, y1) (x2, y2) = atan ((y1 - y2) / (x1 - x2))


drawPassengersOnTrain :: [Passenger] -> Picture
drawPassengersOnTrain [] = blank
drawPassengersOnTrain passenger = drawInARow (take 2 passenger) 0.25 drawPassanger <> translated 0 0.25 (drawPassengersOnTrain (drop 2 passenger))

defaultControlBackground :: Picture
defaultControlBackground = colored gray (solidCircle 0.65) <> colored (dark gray) (solidCircle 0.75)

drawAssetType :: AssetType -> Picture
drawAssetType (LineColor backgroundColor) 
  = colored backgroundColor (solidCircle 0.65)
  <> colored (dark backgroundColor) (solidCircle 0.75)
drawAssetType Train = lettering "T" <> defaultControlBackground
drawAssetType Wagon = lettering "W" <> defaultControlBackground

drawControlsRecursion :: [Control] -> Bool -> Picture
drawControlsRecursion [] _ = blank
drawControlsRecursion ((Control assetType (x, y)):rest) isEnabled
  = translated x y (drawAssetType assetType)
  <> drawControlsRecursion rest isEnabled

drawControls :: GameState -> Picture
drawControls (GameState _ _ _ assets _mode currentTime) 
  = drawControlsRecursion controls isEnabled
  where
    week = floor currentTime `div` 7
    isEnabled = week > length assets - 2 