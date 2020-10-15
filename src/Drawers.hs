{-# LANGUAGE OverloadedStrings #-}
module Drawers where 
import Types
import CodeWorld
import Config
import Data.Text (pack)

-- | Draw background image
backgroundImage :: Picture
backgroundImage = colored (lighter 0.5 brown) (solidRectangle 100 100)

-- | Draw a station accoring to its type
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

-- | Draw a passanger
drawPassanger :: Passenger -> Picture
drawPassanger (Passenger Triangle) = scaled 0.2 0.2 (drawFigure TriangleFigure)
drawPassanger (Passenger Rectangle) = scaled 0.2 0.2 (drawFigure SquareFigure)
drawPassanger (Passenger Circle) = scaled 0.2 0.2 (drawFigure CircleFigure)

-- | Helper for drawStationType, draws a given figure type
drawFigure :: Figure -> Picture
drawFigure TriangleFigure = triangle
  where
    triangle = solidPolygon [(-triangleSize/2, -(median-radius)), (0, radius), (triangleSize/2, -(median-radius))]
    triangleSize = 1.2
    radius = triangleSize * sqrt 3/3
    median = sqrt 3 * triangleSize / 2

drawFigure SquareFigure = solidRectangle 1 1
drawFigure CircleFigure = solidCircle 0.5

-- | Draw given station with its passangers
drawStation :: Station -> Picture
drawStation (Station stationType (x, y) passangers _) 
  = translated x y (drawStationType stationType) <> drawPassengersOnStation (x, y) passangers

-- | helper for drawing a banch of passangers 
drawInARow :: [a] -> Double -> (a->Picture) -> Picture
drawInARow [] _ _ = blank
drawInARow (first:rest) margin drawer = 
  drawer first
  <> translated margin 0 (drawInARow rest margin drawer)

-- | draw list of passangers in given position with a margin on each raw
drawPassengersOnStation :: Position -> [Passenger] -> Picture
drawPassengersOnStation (x, y) passengers = translated (x+0.6) (y+0.6) (passes passengers)
  where
    passes :: [Passenger] -> Picture
    passes [] = blank
    passes passenger = drawInARow (take 10 passenger) 0.25 drawPassanger <> translated 0 0.25 (passes (drop 10 passenger))


-- | Render a list of given objects by a given function
renderObject :: (a -> Picture) -> [a] -> Picture
renderObject func = pictures . map func

-- | Draw a route
drawRoute :: Route -> Picture
drawRoute (Route color station1 station2) = colored color (thickCurve 0.3 positions)
  where
    positions = [station1, station2]

-- | Draw locomotive
drawLocomotive :: Locomotive -> Picture
drawLocomotive (Locomotive trainPassangers _direction others) = drawing
  where
    drawing =
      case others of 
        OnRoute (Route color pos1 pos2) progress -> baseDrawing (getTrainPositionWithProgress progress pos1 pos2) (-(pi / 2 - getAngle pos1 pos2)) color
        TransferTo pos (Route color _ _) -> baseDrawing pos 0 color
        TransferFrom pos (Route color _ _) -> baseDrawing pos 0 color
        Ready pos (Route color _ _) -> baseDrawing pos 0 color
    baseDrawing (x, y) rotation color =  translated x y (rotated rotation (passengers color <> locomotiveBase color))
    locomotiveBase locomotiveColor = colored (light locomotiveColor) (solidRectangle 0.66 1)
    passengers locomotiveColor = translated (-0.13) (-0.25) (colored (lighter 0.4 locomotiveColor) (drawPassengersOnTrain trainPassangers))
      
-- | Helper for getting locomotive's position with respect to the progress
getTrainPositionWithProgress :: Double -> Position -> Position -> Position
getTrainPositionWithProgress percent (x1, y1) (x2, y2) = (x1 * (1 - percent) + x2 * percent, y1 * (1 - percent) + y2 * percent)

-- | helper for calculating an angle of train's rotation
getAngle :: Position -> Position -> Double
getAngle (x1, y1) (x2, y2) = atan ((y1 - y2) / (x1 - x2))

-- | Draw a cross
drawRemoval :: Picture
drawRemoval = smallRectangle <> rotated (pi/2) smallRectangle <> colored gray (solidCircle 0.15) <> colored black (solidCircle 0.20)
  where
    smallRectangle = rotated (pi/4) (colored black (solidRectangle 0.2 0.05))

-- | Helper for drawing passangers on a locomotive (in a smaller batch)
drawPassengersOnTrain :: [Passenger] -> Picture
drawPassengersOnTrain [] = blank
drawPassengersOnTrain passenger = drawInARow (take 2 passenger) 0.25 drawPassanger <> translated 0 0.25 (drawPassengersOnTrain (drop 2 passenger))

-- | Draw default control's background
defaultControlBackground :: Picture
defaultControlBackground = colored gray (solidCircle 0.65) <> colored (dark gray) (solidCircle 0.75)

-- | Draw assets in control panel
drawAssetType :: AssetType -> Picture
drawAssetType (LineColor backgroundColor) 
  = colored backgroundColor (solidCircle 0.65)
  <> colored (dark backgroundColor) (solidCircle 0.75)
drawAssetType Train = lettering "T" <> defaultControlBackground
drawAssetType Wagon = lettering "W" <> defaultControlBackground

-- | Helper for making a list with unique elements
getUnique :: Eq a => [a] -> [a]
getUnique = helper [] 
  where
    helper uniqueList [] = uniqueList
    helper uniqueList (first:rest)
      | first `elem` uniqueList = helper uniqueList rest
      | otherwise = helper (first:uniqueList) rest

-- | Get unique list of colors
getUniqueLinesColors :: [Route] -> [Color]
getUniqueLinesColors routes = getUnique (map (\(Route color _ _)->color) routes)

-- | Get current number of routes used
amountOfLinesUsed :: GameState -> Int
amountOfLinesUsed state = length (getUniqueLinesColors (getRoutes state))

-- | Get current number of locomotives used
amountOfTrainsUsed :: GameState -> Int
amountOfTrainsUsed state = length (getLocomotives state)

-- | Get current number of assets used
getAmountOfAssetsUsed :: GameState -> Int
getAmountOfAssetsUsed state = amountOfLinesUsed state + amountOfTrainsUsed state

-- | Get current week number
getCurrentWeek :: Double -> Int
getCurrentWeek currentTime = floor currentTime `div` 7

-- | Get amount of assets available for a given week
getAmountOfAssets :: Double -> Int
getAmountOfAssets = (+ initialAmountOfAssets) . getCurrentWeek

-- | Drawing function for entire game
drawMode :: GameMode -> Picture
drawMode Play = lettering "Click below to start adding trains or lines"
drawMode Repopulation = lettering "Choose color line where to place the train"
drawMode (Construction _ (Just _)) = lettering "Choose second station"
drawMode (Construction _ Nothing) = lettering "Choose first station"

-- | Check if this asset is available for usage
isAssetAvailable :: Bool -> AssetType -> [Color] -> Bool
isAssetAvailable False _ _ = True
isAssetAvailable _ (LineColor color) colors = color `elem` colors
isAssetAvailable True _ _ = False

-- | Helper for drawing control panel recurcsively
drawControlsRecursion :: [Control] -> [Color] -> Bool -> Picture
drawControlsRecursion [] _ _ = blank
drawControlsRecursion ((Control assetType (x, y)):rest) colors outOfAssets
  = translated x y (resultingPicture <> translated 0.75 0.75 drawRemoval)
  <> drawControlsRecursion rest colors outOfAssets
  where
    
    resultingPicture = scaledIfNotAvailable isAvailable (drawAssetType assetType)
    isAvailable = isAssetAvailable outOfAssets assetType colors

    scaledIfNotAvailable True picture = picture
    scaledIfNotAvailable False picture = scaled 0.5 0.5 picture

drawControlsRecursion((Removal _):rest) colors outOfAssets = drawControlsRecursion rest colors outOfAssets

-- | Draw contlor panel
drawControls :: GameState -> Picture
drawControls state@(GameState _ routes _ mode currentTime) 
  = drawControlsRecursion controls colors outOfAssets
    <> translated 10 6 (scaled 0.8 0.8 (translated 0 1(lettering $ pack $ ("Amount of assets used: "++) $ show $ getAmountOfAssetsUsed state)
    <> translated 0 2 (lettering $ pack $ ("Amount of assets available : " ++) $ show assets)
    <> translated 0 3 (drawMode mode)))
  where
    assets = getAmountOfAssets currentTime
    outOfAssets = getAmountOfAssetsUsed state >= assets
    colors = getUniqueLinesColors routes