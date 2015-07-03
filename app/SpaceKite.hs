{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
-- module SpaceKite(
--   getDistance,
--   isInSegment
--   ) where
module Main where

import Control.Applicative
import Data.List
import Data.Maybe

data Parser a = Parser { parse :: String -> Maybe (a, String) }

instance Monad Parser where
  return x = Parser $ \input -> Just (x, input)
  (>>=) ma f = Parser newParse
    where newParse input =
            let firstParseResult = parse ma input in
            case firstParseResult of
              Nothing -> Nothing
              Just (middleResult, leftString) -> parse (f middleResult) leftString

instance Applicative Parser where
  pure x = Parser $ \input -> Just (x, input)
  (<*>) mf ma = do
    f <- mf
    a <- ma
    return $ f a

instance Functor Parser where
  fmap f ma = do
    a <- ma
    return $ f a

run :: Parser a -> String -> Maybe a
run parser input = fst `fmap` parse parser input

consume :: Parser ()
consume = Parser $
  \input -> case maybeHead input of
               Nothing -> Nothing
               _ -> Just $ ((), drop 1 input)

parseFail :: Parser ()
parseFail = Parser $ \_ -> Nothing

readInt :: Parser Int
readInt = Parser $ listToMaybe `fmap` reads

readWhiteSpace :: Parser ()
readWhiteSpace = do
  isWhite <- isWhiteSpace `fmap` lookAhead
  if isWhite then consume else parseFail
  where
    isWhiteSpace c = c == ' '

lookAhead :: Parser Char
lookAhead = Parser $ \input ->
  case maybeHead input of
    Nothing -> Nothing
    Just c -> Just (c, input)

maybeHead :: [a] -> Maybe a
maybeHead lst = listToMaybe $ take 1 lst

data Header = Header {
  numOfPlanet :: Int,
  numOfSpot :: Int,
  planetRadious :: Int,
  communicationDistance :: Int
} deriving Show

type Position = (Int, Int, Int)

type PlanetPos = Position

type PlayerPos = Position

type SpotPos = Position

data DataSet = DataSet {
  header :: Header,
  planetPoses :: [PlanetPos],
  playerPos :: PlayerPos,
  spotPoses :: [SpotPos]
} deriving Show

readHeader :: Parser Header
readHeader = Header `fmap` numOfPlanet <*> numOfSpot <*> planetRadious <*> communicationDistance
  where
    numOfPlanet = readInt
    numOfSpot = readWhiteSpace *> readInt
    planetRadious = readWhiteSpace *> readInt
    communicationDistance = readWhiteSpace *> readInt

readPos :: Parser Position
readPos = (,,) `fmap` x <*> y <*> z
  where
    x = readInt
    y = readWhiteSpace *> readInt
    z = readWhiteSpace *> readInt

readPositions :: Int -> Parser [Position]
readPositions numOfPosition = sequence $ take numOfPosition $ repeat readPos

readDataSet :: Parser DataSet
readDataSet = do
  header <- readHeader
  planetPoses <- readPositions (numOfPlanet header)
  playerPos <- readPos
  spotPoses <- readPositions (numOfSpot header)
  return $ DataSet header planetPoses playerPos spotPoses

(.-) :: Position -> Position -> Position
(.-) (lx, ly, lz) (rx, ry, rz) = (lx - rx, ly - ry, lz - rz)

dot :: Position -> Position -> Int
dot (lx, ly, lz) (rx, ry, rz) = (lx * rx) + (ly * ry) + (lz * rz)

magnitudeSquare :: Position -> Int
magnitudeSquare (x, y, z) = x * x + y * y + z * z

type Segment = (Position, Position)

getDistanceSquare :: Segment -> Position -> Rational
getDistanceSquare (startPosition, arrivalPosition) planetPosition =
  let vecA = arrivalPosition .- startPosition in
  let vecB = planetPosition .- startPosition in
  let cosThetaSquare = (dotf vecA vecB) * (dotf vecA vecB) / ((magnitudeSquaref vecA) * (magnitudeSquaref vecB)) in
  let sinThetaSquare = 1 - cosThetaSquare in
  sinThetaSquare * (magnitudeSquaref vecB)
    where dotf x y = fromIntegral $ dot x y
          magnitudeSquaref = fromIntegral . magnitudeSquare

isInSegment :: Segment -> Position -> Bool
isInSegment (startPosition, arrivalPosition) planetPosition =
  let vecA = arrivalPosition .- startPosition in
  let vecB = planetPosition .- startPosition in
  let dotAB = dot vecA vecB in
  0 <= dotAB && dotAB <= (magnitudeSquare vecA)

createSegments :: DataSet -> [Segment]
createSegments (DataSet { playerPos = playerPos, spotPoses = spotPoses }) =
  let allPoints = playerPos : spotPoses in
  zip allPoints spotPoses

type Index = Int
isCommunicatable :: DataSet -> Segment -> (Index, PlanetPos) -> Maybe Index
isCommunicatable dataSet segment (index, planetPos) =
  if (distanceSquare <= limit * limit) && inSegment then Just index else Nothing
  where distanceSquare = getDistanceSquare segment planetPos
        comDistance = communicationDistance $ header dataSet
        radious = planetRadious $ header dataSet
        limit = fromIntegral $ radious + comDistance
        inSegment = isInSegment segment planetPos

getCommunicatablePlanets :: DataSet -> [Index]
getCommunicatablePlanets dataSet = sort $ nub $ do
  indexWithPlanet <- zip [1..] $ planetPoses dataSet
  segment <- createSegments dataSet
  let maybeIndex = isCommunicatable dataSet segment indexWithPlanet
  maybeToList maybeIndex

getDistanceSquares :: DataSet -> [Rational]
getDistanceSquares dataSet = do
  segment <- createSegments dataSet
  spotPos <- planetPoses dataSet
  return $ getDistanceSquare segment spotPos

data Direction = Approaching | GoingAway

data DistanceAndDirection = DistanceAndDirection {
  distanceSquare :: Int,
  direction :: Direction,
  planetIndex :: Index,
  planetPosition :: Position
}

getDirection :: (PlayerPos, SpotPos) -> PlanetPos -> Direction
getDirection (playerPos, spotPos) planetPos =
  let vecA = spotPos .- playerPos in
  let vecB = planetPos .- playerPos in
  let dotAB = dot vecA vecB in
  if dotAB >= 0 then GoingAway else Approaching

getDistanceAndDirection :: (PlayerPos, SpotPos) -> PlanetPos -> DistanceAndDirection
getDistanceAndDirection segment@(playerPos, spotPos) planetPos =
  DistanceAndDirection {
    distanceSquare = magnitudeSquare diffVec,
    direction = getDirection segment planetPos
  }
    where diffVec = planetPos .- playerPos

-- getNextSpecificPoint :: (PlayerPos, SpotPos) -> Reader DataSet Position
-- getNextSpecificPoint (playerPos, spotPos) =

class Monad m => MonadReader e m | m -> e where
  ask :: m e

data Reader e a = Reader { runReader :: e -> a }

instance Monad (Reader e) where
  return x = Reader $ \_ -> x
  (>>=) (Reader reader) f = Reader $ \env -> runReader (f (reader env)) env

instance MonadReader e (Reader e) where
  ask = Reader $ id

instance Applicative (Reader e) where
  pure x = return x
  (<*>) mf ma = do
    f <- mf
    a <- ma
    return $ f a

instance Functor (Reader e) where
  fmap f ma = do
    a <- ma
    return $ f a

getPlanetsInPoint :: PlayerPos -> Reader DataSet [Index]
getPlanetsInPoint playerPos = return [] -- FIXME: Not Implemented.

getNextSpecificPoint :: Segment -> Reader DataSet Position
getNextSpecificPoint segment = return (0, 0, 0) -- FIXME: Not Implemented.

findPlanetsInSegment :: Segment -> Reader DataSet [Index]
findPlanetsInSegment segment@(startPos, endPos)
  | startPos == endPos = getPlanetsInPoint startPos
  | otherwise = do
    dataSet <- ask
    planetsInPoint <- getPlanetsInPoint startPos
    nextSpecificPoint <- getNextSpecificPoint segment
    ((++) planetsInPoint) `fmap` (findPlanetsInSegment (nextSpecificPoint, endPos))

doLogic :: Int -> IO ()
doLogic iteration = do
  allInput <- getContents
  let readDataSets = sequence $ take iteration $ repeat readDataSet
  let dataSets = run readDataSets allInput
  let maybeResults = (map getCommunicatablePlanets) `fmap` dataSets
  let strs = map resultToStr `fmap` maybeResults
  -- print strs
  case strs of
    Nothing -> return ()
    Just xs -> mapM_ putStrLn xs
  where resultToStr lst =
          let lstWithCount = length lst : lst in
          let strs = map show lstWithCount in
          concat $ intersperse " " strs
  -- print $ show $ dataSets
  -- print $ show $ (map getCommunicatablePlanets) `fmap` dataSets
  -- print $ show $ (map getDistanceSquares) `fmap` dataSets

main :: IO ()
main = do
  iteration <- read `fmap` getLine
  doLogic iteration
