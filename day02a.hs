import Data.List.Split

main :: IO ()
main = do
  input <- readFile "input"
  print $ sum $ map (pair . map nums) $ map cols $ lines input

nums :: String -> Int
nums s = read s :: Int

cols :: String -> [String]
cols s = filter ((/=) "") $ splitOn " " s

pair :: [Int] -> Int
pair xs = mx - mn
  where
    mx = maximum xs
    mn = minimum xs
