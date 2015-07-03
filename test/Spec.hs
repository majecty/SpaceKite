import SpaceKite

describe :: String -> Bool -> IO ()
describe message True = putStrLn "."
describe message False = putStrLn $ "Fail test " ++ message

should :: (Eq a) => a -> a -> Bool
should = (==)

distance = getDistance ((0, 0, 0), (10, 0, 0)) (0, 5, 0)

main :: IO ()
main = do
  describe "check distance" $
    5.0 `should` distance
