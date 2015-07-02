module Main where

doLogic :: Integer -> IO ()
doLogic iteration =
  print $ show $ iteration

main :: IO ()
main = do
  print "Start"
  iteration <- read `fmap` getLine
  doLogic iteration
  print "hi"
