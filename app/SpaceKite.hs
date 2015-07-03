module SpaceKite(
  getDistance,
  isInSegment
  ) where

import Control.Applicative
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

magnitude :: Floating a => Position -> a
magnitude (x, y, z) = sqrt (fx * fx + fy * fy + fz * fz)
  where fx = fromIntegral x
        fy = fromIntegral y
        fz = fromIntegral z

magnitudeSquare :: Position -> Int
magnitudeSquare (x, y, z) = x * x + y * y + z * z

type Segment = (Position, Position)

getDistance :: Floating a => Segment -> Position -> a
getDistance (startPosition, arrivalPosition) planetPosition =
  let vecA = arrivalPosition .- startPosition in
  let vecB = planetPosition .- startPosition in
  let cosTheta = dotf vecA vecB / ((magnitude vecA) * (magnitude vecB)) in
  let sinTheta = sqrt $ 1 - (cosTheta * cosTheta) in
  sinTheta * (magnitude vecB)
    where dotf x y = fromIntegral $ dot x y

isInSegment :: Segment -> Position -> Bool
isInSegment (startPosition, arrivalPosition) planetPosition =
  let vecA = arrivalPosition .- startPosition in
  let vecB = planetPosition .- startPosition in
  let dotAB = dot vecA vecB in
  0 <= dotAB && dotAB <= (magnitudeSquare vecA)

doLogic :: Int -> IO ()
doLogic iteration = do
  allInput <- getContents
  let readDataSets = sequence $ take iteration $ repeat readDataSet
  let dataSets = run readDataSets allInput
  print $ show $ dataSets

main :: IO ()
main = do
  print "Start"
  iteration <- read `fmap` getLine
  doLogic iteration
  print "hi"
