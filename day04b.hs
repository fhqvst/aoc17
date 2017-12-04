import Data.List.Split
import Data.List

main :: IO ()
main = do
  input <- readFile "input"
  print $ length $ filter (\ls -> ls == nub ls) $ map (map sort) $ map words $ lines input
