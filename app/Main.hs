module Main where

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

readInteger :: Parser Integer
readInteger = Parser $ listToMaybe `fmap` reads

data Header = Header {
  numOfPlanet :: Integer,
  numOfSpot :: Integer,
  planetRadious :: Integer,
  communicationDistance :: Integer
} deriving Show

readHeader :: String -> Header
readHeader input =
  let (numOfPlanet, middle1) = head $ reads input in
  let (' ', middle2) = head $ reads middle1 in
  let (numOfSpot, middle3) = head $ reads middle2 in
  let (' ', middle4) = head $ reads middle3 in
  let (planetRadious, middle5) = head $ reads middle4 in
  let (' ', middle6) = head $ reads middle5 in
  let (communicationDistance, middle7) = head $ reads middle6 in
  Header numOfPlanet numOfSpot planetRadious communicationDistance

doLogic :: Integer -> IO ()
doLogic iteration = do
  line <- getLine
  let header = readHeader line
  print $ show $ iteration
  print $ show $ header

main :: IO ()
main = do
  print "Start"
  iteration <- read `fmap` getLine
  doLogic iteration
  print "hi"
