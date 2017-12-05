import Data.List.Split
import Data.List
import Data.Char

main :: IO ()
main = do
  input <- readFile "input"
  let is = map parseInt $ lines input
  let (_, j, _) = jump (0, 0, is)
  print (j - 1)

jump :: (Int, Int, [Int]) -> (Int, Int, [Int])
jump (i, j, is) = case i >= length is of
  True  -> (i, j + 1, is)
  False -> jump (i', j', is')
      where
        i' = i + (is !! i)
        is' = put i ((is !! i) + 1) is
        j' = j + 1

parseInt :: String -> Int
parseInt s = read s :: Int

put :: Int -> Int -> [Int] -> [Int]
put i x is = a ++ (x:bs)
  where
    (a, (b:bs)) = splitAt i is
