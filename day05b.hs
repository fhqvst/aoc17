import Control.Monad.ST
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M

solve :: (Int -> Int) -> String -> Int
solve op input = runST $ V.thaw nums >>= jump 0 0
  where
    nums = V.fromList $ map read $ lines input
    jump i j ns
      | i < 0 || i >= M.length ns = return j
      | otherwise = do
          x <- M.read ns i
          M.write ns i $ op x
          jump (i + x) (j + 1) ns

main :: IO ()
main = do
  input <- readFile "input"
  print $ solve (\x -> if x >= 3 then (x - 1) else (x + 1)) input
