import SpaceKite

describe :: String -> Bool -> IO ()
describe message True = putStrLn "."
describe message False = putStrLn $ "Fail test " ++ message

should :: (Eq a) => a -> a -> Bool
should = (==)

main :: IO ()
main = do
  describe "check distance" $
    5.0 `should` getDistance ((0, 0, 0), (10, 0, 0)) (0, 5, 0)
  describe "check isInSegment" $
    True `should` isInSegment ((0, 0, 0), (10, 0, 0)) (0, 5, 0)
  describe "check False case of isInSegment" $
    False `should` isInSegment ((0, 0, 0), (10, 0, 0)) (-1, 0, 0)
