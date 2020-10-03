module Drawers where 
import  Types
import CodeWorld
import Data.Text (pack)

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
drawRoute (Route color station1 station2) = colored color (thickCurve 0.3 positions)
  where
    positions = [station1, station2]

-- TODO: remove debug stuff
drawLocomotive :: Locomotive -> Picture
drawLocomotive locomotive =
  translated x y (rotated (- angle) (passengers <> locomotiveBase))
    <> translated (-9) (-9) (lettering (pack $ show progress))
    <> translated (-9) (-8) (lettering (pack $ show $ getLocomotiveDirection locomotive))
    <> translated (-9) (-7) (lettering (pack $ show pos1))
  where
    angle = pi / 2 - getAngle pos1 pos2
    progress = getLocomotivePosition locomotive
    (x, y) = getTrainPositionWithProgress progress pos1 pos2

    (Route locomotiveColor pos1 pos2) = getLocomotiveRoute locomotive
    locomotiveBase = colored (light locomotiveColor) (solidRectangle 0.66 1)
    passengers = translated (-0.13) (-0.25) (colored (lighter 0.4 locomotiveColor) (drawPassengersOnTrain (getLocomotivePassengers locomotive)))

getTrainPositionWithProgress :: Double -> Position -> Position -> Position
getTrainPositionWithProgress percent (x1, y1) (x2, y2) = (x1 * (1 - percent) + x2 * percent, y1 * (1 - percent) + y2 * percent)

getAngle :: Position -> Position -> Double
getAngle (x1, y1) (x2, y2) = atan ((y1 - y2) / (x1 - x2))


drawPassengersOnTrain :: [Passenger] -> Picture
drawPassengersOnTrain [] = blank
drawPassengersOnTrain passenger = drawInARow (take 2 passenger) 0.25 drawPassanger <> translated 0 0.25 (drawPassengersOnTrain (drop 2 passenger))
