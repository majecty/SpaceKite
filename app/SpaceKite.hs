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
import Data.Ord
import Debug.Trace

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

readDouble :: Parser Double
readDouble = fromIntegral `fmap` readInt

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

type Position = (Double, Double, Double)

data PlanetPos = PlanetPos { index :: Index, position :: Position }
  deriving (Eq, Show)

instance Ord PlanetPos where
  (<=) (PlanetPos lhsIndex  _) (PlanetPos rhsIndex _) = lhsIndex <= rhsIndex

type PlayerPos = Position

type SpotPos = Position

data DataSet = DataSet {
  header :: Header,
  planetPoses :: [PlanetPos],
  playerPos :: PlayerPos,
  spotPoses :: [SpotPos]
} deriving Show

fromTuple :: (Index, Position) -> PlanetPos
fromTuple (index, position) = PlanetPos index position

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
    x = readDouble
    y = readWhiteSpace *> readDouble
    z = readWhiteSpace *> readDouble

readPositions :: Int -> Parser [Position]
readPositions numOfPosition = sequence $ take numOfPosition $ repeat readPos

readDataSet :: Parser DataSet
readDataSet = do
  header <- readHeader
  planetPoses <- readPositions (numOfPlanet header)
  playerPos <- readPos
  spotPoses <- readPositions (numOfSpot header)
  return $ DataSet header (positionToPlanetPos planetPoses) playerPos spotPoses
    where positionToPlanetPos positions = map fromTuple (zip [1..] positions)

(.-) :: Position -> Position -> Position
(.-) (lx, ly, lz) (rx, ry, rz) = (lx - rx, ly - ry, lz - rz)

dot :: Position -> Position -> Double
dot (lx, ly, lz) (rx, ry, rz) = (lx * rx) + (ly * ry) + (lz * rz)

interpolate :: Position -> Position -> Double -> Position
interpolate (lx, ly, lz) (rx, ry, rz) ratio = (interpolate1 lx rx, interpolate1 ly ry, interpolate1 lz rz)
  where interpolate1 a b = a * (1 - ratio) + b * ratio

unInterpolate :: Segment -> Position -> Double
unInterpolate (startPos, endPos) otherPos =
  let vecA = endPos .- startPos in
  let vecB = otherPos .- startPos in
  let dotAB = dot vecA vecB in
  dotAB / (magnitudeSquare vecA)

debugUnInterpolate :: Segment -> Position -> Double
debugUnInterpolate segment otherPos =
  let output = unInterpolate segment otherPos in
  trace
    ("unInterPolate : segment " ++ (show segment) ++
      ", otherPos " ++ (show otherPos) ++
      ", result " ++ (show output)
    )
  output

magnitude :: Position -> Double
magnitude (x, y, z) = sqrt $ x * x + y * y + z * z

magnitudeSquare :: Position -> Double
magnitudeSquare (x, y, z) = x * x + y * y + z * z

distance :: Position -> Position -> Double
distance posX posY = magnitude (posX .- posY)

distanceSquare :: Position -> Position -> Double
distanceSquare posX posY = magnitudeSquare (posX .- posY)

type Segment = (Position, Position)

createSegments :: DataSet -> [Segment]
createSegments (DataSet { playerPos = playerPos, spotPoses = spotPoses }) =
  let allPoints = playerPos : spotPoses in
  zip allPoints spotPoses

type Index = Int

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

getLimit :: Reader DataSet Int
getLimit = do
  dataSet <- ask
  let radious = planetRadious $ header dataSet
  let maxDistance = communicationDistance $ header dataSet
  return $ radious + maxDistance

epsilon :: Double
epsilon = 0.00000001

isEqual :: Double -> Double -> Bool
isEqual a b = abs (a - b) < epsilon

getPlanets :: Reader DataSet [PlanetPos]
getPlanets = planetPoses `fmap` ask

findPlanetsInSegment :: Segment -> Reader DataSet [PlanetPos]
findPlanetsInSegment segment = findPlanetsInSegmentIng segment 0

findInCurrentPos :: Segment -> Double -> Reader DataSet [PlanetPos]
findInCurrentPos segment@(startPos, endPos) ratio = do
  planets <- getPlanets
  limit <- getLimit
  let planetWithDistances = map makePlanetDistance planets
  let sortedPlanets = sortByDistance planetWithDistances
  let minimumDistance = snd $ head sortedPlanets
  if minimumDistance <= (fromIntegral $ limit * limit)
     then return $ map fst $ filter (filterLimit minimumDistance) sortedPlanets
     else return []
  where currentPos = interpolate startPos endPos ratio
        sortByDistance = sortBy (\(_, d1) (_, d2) -> compare d1 d2)
        makePlanetDistance planet@(PlanetPos _ pos) = (planet, (distanceSquare pos currentPos))
        filterLimit limit (_, distance) = (distance <= limit)

findPlanetsInSegmentIng :: Segment -> Double -> Reader DataSet [PlanetPos]
findPlanetsInSegmentIng segment@(startPos, endPos) ratio
  | isEqual ratio 1 = findInCurrentPos segment 1
  | otherwise = (++) `fmap` (findInCurrentPos segment ratio) <*> findPlanetsInSegmentIng segment nextRatio
      where nextRatio = 1

findAllPlanets :: Reader DataSet [PlanetPos]
findAllPlanets = do
  dataSet <- ask
  let segments = createSegments dataSet
  planetPoses <- concat `fmap` mapM findPlanetsInSegment segments
  return $ sort $ nub $ planetPoses

runOnce :: DataSet -> [PlanetPos]
runOnce dataSet = runReader findAllPlanets dataSet

doLogic :: Int -> IO ()
doLogic iteration = do
  allInput <- getContents
  let readDataSets = sequence $ take iteration $ repeat readDataSet
  let maybeDataSets = run readDataSets allInput
  let results = case maybeDataSets of
                  Nothing -> []
                  Just dataSets -> map runOnce dataSets
  print $ show $ fmap index `map` results
  -- where resultToStr lst =
  --         let lstWithCount = length lst : lst in
  --         let strs = map show lstWithCount in
  --         concat $ intersperse " " strs
  print $ show $ maybeDataSets

main :: IO ()
main = do
  iteration <- read `fmap` getLine
  doLogic iteration
