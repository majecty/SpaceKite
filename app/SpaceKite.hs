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
  return $ DataSet header planetPoses playerPos spotPoses

(.-) :: Position -> Position -> Position
(.-) (lx, ly, lz) (rx, ry, rz) = (lx - rx, ly - ry, lz - rz)

dot :: Position -> Position -> Rational
dot (lx, ly, lz) (rx, ry, rz) = (lx * rx) + (ly * ry) + (lz * rz)

magnitudeSquare :: Position -> Rational
magnitudeSquare (x, y, z) = x * x + y * y + z * z

distanceSquare :: Position -> Position -> Rational
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

newtype Indexed a = Indexed (Index, a)
  deriving Show

instance Functor Indexed where
  fmap f (Indexed (index, a)) = Indexed (index, f a)

getValue :: Indexed a -> a
getValue (Indexed (_, a)) = a

getIndex :: Indexed a -> Index
getIndex (Indexed (index, _)) = index

getLimit :: Reader DataSet Int
getLimit = do
  dataSet <- ask
  let radious = planetRadious $ header dataSet
  let maxDistance = communicationDistance $ header dataSet
  return $ radious + maxDistance

getPlanetsWithIndex :: Reader DataSet [(Indexed Position)]
getPlanetsWithIndex = do
  dataSet <- ask
  return $ map Indexed $ zip [1..] (planetPoses dataSet)

getPlanetsInPoint :: PlayerPos -> Reader DataSet [Index]
getPlanetsInPoint playerPos = do
  limit <- getLimit
  planetsWithIndex <- getPlanetsWithIndex
  let distanceSquaresWithIndex = (fmap distanceSquareFrom) `map` planetsWithIndex
  let distanceSquares = map getValue distanceSquaresWithIndex
  let minDistanceSquare = minimumBy compare distanceSquares
  let minimumIndexes = map getIndex $ filter (isEqualInternalValue minDistanceSquare) distanceSquaresWithIndex 
  return minimumIndexes
    where distanceSquareFrom = distanceSquare playerPos
          isEqualInternalValue value = (== value) . getValue

getNextSpecificPoint :: Segment -> Reader DataSet Position
getNextSpecificPoint segment@(_, endPos) = return endPos -- FIXME: Not Implemented.

findPlanetsInSegment :: Segment -> Reader DataSet [Index]
findPlanetsInSegment segment@(startPos, endPos)
  | startPos == endPos = getPlanetsInPoint startPos
  | otherwise = do
    planetsInPoint <- getPlanetsInPoint startPos
    nextSpecificPoint <- getNextSpecificPoint segment
    ((++) planetsInPoint) `fmap` (findPlanetsInSegment (nextSpecificPoint, endPos))

findAllPlanets :: Reader DataSet [Index]
findAllPlanets = do
  dataSet <- ask
  let segments = createSegments dataSet
  planetIndexes <- concat `fmap` mapM findPlanetsInSegment segments
  return $ sort $ nub $ planetIndexes

runOnce :: DataSet -> [Index]
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
