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

readRational :: Parser Rational
readRational = fromIntegral `fmap` readInt

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

type Position = (Rational, Rational, Rational)

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

newtype Sqrt = Sqrt Rational
  deriving (Eq, Ord, Show)

fromTuple :: (Index, Position) -> PlanetPos
fromTuple (index, position) = PlanetPos index position

square :: Sqrt -> Rational
square (Sqrt a) = a

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
    x = readRational
    y = readWhiteSpace *> readRational
    z = readWhiteSpace *> readRational

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

dot :: Position -> Position -> Rational
dot (lx, ly, lz) (rx, ry, rz) = (lx * rx) + (ly * ry) + (lz * rz)

interpolate :: Position -> Position -> Rational -> Position
interpolate (lx, ly, lz) (rx, ry, rz) ratio = (interpolate1 lx rx, interpolate1 ly ry, interpolate1 lz rz)
  where interpolate1 a b = a * (1 - ratio) + b * ratio

unInterpolate :: Segment -> Position -> Rational
unInterpolate (startPos, endPos) otherPos =
  let vecA = endPos .- startPos in
  let vecB = otherPos .- startPos in
  let dotAB = dot vecA vecB in
  dotAB / (square $ magnitude vecA)

debugUnInterpolate :: Segment -> Position -> Rational
debugUnInterpolate segment otherPos =
  let output = unInterpolate segment otherPos in
  trace
    ("unInterPolate : segment " ++ (show segment) ++
      ", otherPos " ++ (show otherPos) ++
      ", result " ++ (show output)
    )
  output

magnitude :: Position -> Sqrt
magnitude (x, y, z) = Sqrt $ x * x + y * y + z * z

distance :: Position -> Position -> Sqrt
distance posX posY = magnitude (posX .- posY)

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

getPlanets :: Reader DataSet [PlanetPos]
getPlanets = planetPoses `fmap` ask

getPlanetsInPoint :: PlayerPos -> Reader DataSet [PlanetPos]
getPlanetsInPoint playerPos = do
  limit <- getLimit
  planetPoses <- getPlanets
  let distances = distanceFromPlanet `map` planetPoses
  let min = minDistance distances
  return $ findPlanetByDistance min planetPoses
   where distanceFromPlanet = (distance playerPos) . position
         minDistance distances = foldr1 min distances
         findPlanetByDistance minD = filter ((== minD) . distanceFromPlanet)

data Direction = Approaching | GoingAway
  deriving (Eq, Show)

getDirection :: Segment -> PlanetPos -> Direction
getDirection (currentPos, endPos) (PlanetPos _ planetPos) =
  let vecA = endPos .- currentPos in
  let vecB = planetPos .- currentPos in
  let dotAB = dot vecA vecB in
  if dotAB > 0 then Approaching else GoingAway

isApproaching :: Segment -> PlanetPos -> Bool
isApproaching segment planetPos = (== Approaching) $ getDirection segment planetPos

getInflectionPoint :: Segment -> PlanetPos -> Position
getInflectionPoint segment@(startPos, endPos) (PlanetPos _ planetPos) =
  let vecA = endPos .- startPos in
  let vecB = planetPos .- startPos in
  let dotAB = dot vecA vecB in
  let ratio = dotAB / (square $ magnitude vecA) in
  interpolate startPos endPos ratio

getNextSpecificPoint :: Segment -> Rational -> Reader DataSet Rational
--getNextSpecificPoint segment@(_, endPos) currentRatio = return 1 -- FIXME: Not Implemented.
getNextSpecificPoint segment@(startPos, endPos) currentRatio = do
  nearestPlanets <- getPlanetsInPoint currentPos
  let isAnyApproaching = any (isApproaching currentSegment) nearestPlanets
  case isAnyApproaching of
    True -> return $ unInterpolate segment $ nearest candidates
              where candidates = endPos : (farthest' (getInflectionPoint segment `map` nearestApproachings))
                    nearestApproachings = filter (isApproaching currentSegment) nearestPlanets
                    farthest' [] = []
                    farthest' xs = maximumBy compareByDistance xs : []
                    nearest = minimumBy compareByDistance
                    compareByDistance lPos rPos = compare (distance startPos lPos) (distance startPos rPos)
    False -> return 1
  where currentPos = interpolate startPos endPos currentRatio
        currentSegment = (currentPos, endPos)
  -- is there approaching shortest planet?

findPlanetsInSegment :: Rational -> Segment -> Reader DataSet [PlanetPos]
findPlanetsInSegment 1 (_, endPos) = getPlanetsInPoint endPos
findPlanetsInSegment currentRatio segment@(startPos, endPos) = do
  planetsInPoint <- getPlanetsInPoint $ interpolate startPos endPos currentRatio
  nextRatio <- getNextSpecificPoint segment currentRatio
  ((++) planetsInPoint) `fmap` (findPlanetsInSegment nextRatio segment)

findAllPlanets :: Reader DataSet [PlanetPos]
findAllPlanets = do
  dataSet <- ask
  let segments = createSegments dataSet
  planetPoses <- concat `fmap` mapM (findPlanetsInSegment 0) segments
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
  print $ show $ results
  -- where resultToStr lst =
  --         let lstWithCount = length lst : lst in
  --         let strs = map show lstWithCount in
  --         concat $ intersperse " " strs
  print $ show $ maybeDataSets

main :: IO ()
main = do
  iteration <- read `fmap` getLine
  doLogic iteration
