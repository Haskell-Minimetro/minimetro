{-# LANGUAGE OverloadedStrings #-}

module Drawers where 

import qualified Data.Text as Text   (pack)
import qualified CodeWorld as CW
import           Types
import           Config

backgroundImage :: CW.Picture
backgroundImage = CW.colored (CW.lighter 0.5 CW.brown) (CW.solidRectangle 100 100)


drawStationType :: StationType -> CW.Picture
drawStationType Triangle =
  CW.scaled 0.85 0.85 (CW.colored CW.white (drawFigure TriangleFigure))
  <> CW.colored CW.black (drawFigure TriangleFigure)
drawStationType Rectangle =
  CW.scaled 0.85 0.85 (CW.colored CW.white (drawFigure SquareFigure))
  <> CW.colored CW.black (drawFigure SquareFigure)
drawStationType Circle =
  CW.scaled 0.85 0.85 (CW.colored CW.white (drawFigure CircleFigure))
  <> CW.colored CW.black (drawFigure CircleFigure)

drawPassanger :: Passenger -> CW.Picture
drawPassanger (Passenger Triangle) = CW.scaled 0.2 0.2 (drawFigure TriangleFigure)
drawPassanger (Passenger Rectangle) = CW.scaled 0.2 0.2 (drawFigure SquareFigure)
drawPassanger (Passenger Circle) = CW.scaled 0.2 0.2 (drawFigure CircleFigure)

-- TODO: fix this triangle draw wrong
drawFigure :: Figure -> CW.Picture
drawFigure TriangleFigure = triangle
  where
    triangle = CW.solidPolygon [(-triangleSize/2, -(median-radius)), (0, radius), (triangleSize/2, -(median-radius))]
    triangleSize = 1.2
    radius = triangleSize * sqrt 3/3
    median = sqrt 3 * triangleSize / 2

drawFigure SquareFigure = CW.solidRectangle 1 1
drawFigure CircleFigure = CW.solidCircle 0.5

drawCurrentDate :: Double -> CW.Picture
drawCurrentDate _time = CW.blank



drawStation :: Station -> CW.Picture
drawStation (Station stationType (x, y) passangers _) 
  = CW.translated x y (drawStationType stationType) <> drawPassengersOnStation (x, y) passangers


drawInARow :: [a] -> Double -> (a->CW.Picture) -> CW.Picture
drawInARow [] _ _ = CW.blank
drawInARow (first:rest) margin drawer = 
  drawer first
  <> CW.translated margin 0 (drawInARow rest margin drawer)

drawPassengersOnStation :: Position -> [Passenger] -> CW.Picture
drawPassengersOnStation (x, y) passengers = CW.translated (x+0.5) (y+0.5) (passes passengers)
  where
    passes :: [Passenger] -> CW.Picture
    passes [] = CW.blank
    passes passenger = drawInARow (take 10 passenger) 0.25 drawPassanger <> CW.translated 0 0.25 (passes (drop 10 passenger))


-- | Render a list of given objects by a given function
renderObject :: (a -> CW.Picture) -> [a] -> CW.Picture
renderObject func = CW.pictures . map func


drawRoute :: Route -> CW.Picture
drawRoute (Route color station1 station2) = CW.colored color (CW.thickCurve 0.3 positions) -- <> CW.translated x y (CW.colored red $ CW.solidCircle 0.7)
  where
    positions = [station1, station2]

drawLocomotive :: Locomotive -> CW.Picture
drawLocomotive (Locomotive trainPassangers _direction others) = drawing
  -- <> CW.translated (-9) (-8) (lettering (pack $ show  direction))
  -- <> CW.translated (-9) (-9) (lettering (pack $ show  others))
  where
    drawing =
      case others of 
        OnRoute (Route color pos1 pos2) progress -> baseDrawing (getTrainPositionWithProgress progress pos1 pos2) (-(pi / 2 - getAngle pos1 pos2)) color
        TransferTo pos color -> baseDrawing pos 0 color
        TransferFrom pos color -> baseDrawing pos 0 color
        Ready pos color -> baseDrawing pos 0 color
    baseDrawing (x, y) rotation color =  CW.translated x y (CW.rotated rotation (passengers color <> locomotiveBase color))
    locomotiveBase locomotiveColor = CW.colored (CW.light locomotiveColor) (CW.solidRectangle 0.66 1)
    passengers locomotiveColor = CW.translated (-0.13) (-0.25) (CW.colored (CW.lighter 0.4 locomotiveColor) (drawPassengersOnTrain trainPassangers))
      

getTrainPositionWithProgress :: Double -> Position -> Position -> Position
getTrainPositionWithProgress percent (x1, y1) (x2, y2) = (x1 * (1 - percent) + x2 * percent, y1 * (1 - percent) + y2 * percent)

getAngle :: Position -> Position -> Double
getAngle (x1, y1) (x2, y2) = atan ((y1 - y2) / (x1 - x2))


drawPassengersOnTrain :: [Passenger] -> CW.Picture
drawPassengersOnTrain [] = CW.blank
drawPassengersOnTrain passenger = drawInARow (take 2 passenger) 0.25 drawPassanger <> CW.translated 0 0.25 (drawPassengersOnTrain (drop 2 passenger))


defaultControlBackground :: CW.Picture
defaultControlBackground = CW.colored CW.gray (CW.solidCircle 0.65) <> CW.colored (CW.dark CW.gray) (CW.solidCircle 0.75)

drawAssetType :: AssetType -> CW.Picture
drawAssetType (LineColor backgroundColor) 
  = CW.colored backgroundColor (CW.solidCircle 0.65)
  <> CW.colored (CW.dark backgroundColor) (CW.solidCircle 0.75)
drawAssetType Train = CW.lettering "🚅" <> defaultControlBackground
drawAssetType Wagon = CW.lettering "🚟" <> defaultControlBackground

drawControlsRecursion :: [Control] -> Bool -> CW.Picture
drawControlsRecursion [] _ = CW.blank
drawControlsRecursion ((Control assetType (x, y)):rest) isEnabled
  = CW.translated x y (drawAssetType assetType)
  <> drawControlsRecursion rest isEnabled

drawControls :: GameState -> CW.Picture
drawControls (GameState _ _ _ assets _mode currentTime) 
  = drawControlsRecursion controls isEnabled
  -- = translated translateFactor 0 $ CW.scaled scaleFactor scaleFactor $ drawInARow lineColors 2 drawControl
  -- <> CW.lettering mode
  where
    week = floor currentTime `div` 7
    isEnabled = week > length assets - 2 

-- | Draws game state and additional notes
-- | everything we need in the game is displayed according to the state
-- | + additional information
drawGameState :: GameState -> CW.Picture
drawGameState gameState =
  renderObject drawStation (getStations gameState)
    <> CW.translated (-9) (-6) (CW.lettering (Text.pack $ show (getCurrentMode gameState)))
    <> CW.translated (-9) (-4) (CW.lettering (Text.pack $ show (length (getRoutes gameState))))
    <> drawControls gameState
    <> renderObject drawLocomotive (getLocomotives gameState)
    <> renderObject drawRoute (getRoutes gameState)
    <> backgroundImage
