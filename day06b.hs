import qualified Data.Map as Map
import Data.Map as M hiding (map, (!))
import Data.Vector as V hiding (zip, map, modify)
import Data.Function (on)

import Control.Monad
import Control.Monad.State

input = "14 0 15 12 11 11 3 5 1 6 8 4 9 1 8 4"
-- input = "0 2 7 0"

data Env = Env { banks :: Vector Int, seen :: Map (Vector Int) Int, runs :: Int }

main :: IO ()
main = do
  let env = Env {
    banks = V.fromList $ map (read :: String -> Int) $ words input,
    seen = M.insert (V.fromList $ map (read :: String -> Int) $ words input) 0 M.empty,
    runs = 0
  }
  let env' = execState reallocate env
  let seen' = M.insert (banks env') 0 M.empty
  let env'' = execState reallocate $ execState (modify (\e -> e{ runs = 0, seen = seen' })) env'
  print $ runs env''

reallocate :: State Env ()
reallocate = do
  env <- get

  let bs = banks env
  let i = maxIndex bs
  let v = bs ! i

  let bs' = bs // [(i, 0)]
  let bs'' = V.accum (+) bs' $ zip (map (loop bs') [(i + 1)..(i + v)]) (Prelude.replicate 9999999 1)
  
  modify (\e -> e{ banks = bs'', runs = 1 + (runs e) })

  case (M.member bs'' (seen env)) of
    True -> return ()
    False -> do
      modify (\e -> e{ seen = (M.insert bs'' 0 (seen env)) })
      env <- get
      reallocate

loop :: Vector a -> Int -> Int
loop vs i = i `mod` V.length vs
