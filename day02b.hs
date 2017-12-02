import Data.List.Split
import Data.List

main :: IO ()
main = do
  input <- readFile "input"
  print $ sum $ map (pair . reverse . sort . map nums) $ map cols $ lines input

nums :: String -> Int
nums s = read s :: Int

cols :: String -> [String]
cols s = filter ((/=) "") $ splitOn " " s

pair :: [Int] -> Int
pair (x:xs) = case find evenlyDiv xs of
  Just y -> x `div` y
  Nothing -> pair xs
  where
    evenlyDiv y = (x `mod` y) == 0
