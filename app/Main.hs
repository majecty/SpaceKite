module Main where

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

consume :: Parser ()
consume = Parser $
  \input -> case maybeHead input of
               Nothing -> Nothing
               _ -> Just $ ((), drop 1 input)

parseFail :: Parser ()
parseFail = Parser $ \_ -> Nothing

readInteger :: Parser Integer
readInteger = Parser $ listToMaybe `fmap` reads

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
