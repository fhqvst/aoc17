import Data.List as L
import Data.Either
import Data.Maybe
import Data.List.Split
import qualified Data.Map as Map
import Data.Map as M hiding (map, filter)

data Disc = Disc { name :: String, weight :: Int, stack :: [String] }
  deriving Show

data Discs = Discs (Disc, [Discs])
  deriving Show

main :: IO ()
main = do
  input <- map parseDisc <$> lines <$> readFile "input"
  let n2d = M.fromList $ map (\d -> (name d, d)) input
  let discs = buildRecursive n2d $ getBottom input
  case checkWeights discs of
    Left xs -> print $ xs
    Right y -> print $ "all good"

buildRecursive :: Map String Disc -> String -> Discs
buildRecursive n2d n = Discs (d, map (buildRecursive n2d) (stack d))
  where
    d = fromJust $ M.lookup n n2d

checkWeights :: Discs -> Either Int Int
checkWeights (Discs (d, [])) = Right (weight d)
checkWeights (Discs (d, ds)) = if (L.null (lefts cs))
  then if allEqual (rights cs)
    then Right (L.foldl (+) (weight d) (rights cs))
    else do
      let r = rights cs

      -- unique weights
      let weights = nub $ rights cs

      -- assume length == 2
      let diff = abs $ (head weights) - (last weights)

      -- find anomaly weight
      let anomaly = if (length (fst t)) < (length (snd t)) then fst t else snd t where t = span (\x -> (x == head r)) r

      -- ... and the actual program
      let Discs (d',ds') = ds !! (fromJust $ L.elemIndex (head anomaly) r)

      -- return program weight - difference
      Left $ (weight d') - diff

  else Left $ last $ lefts cs
  where
    cs = map checkWeights ds

allEqual :: (Eq a) => [a] -> Bool
allEqual xs = and $ map (== head xs) (tail xs)

parseDisc :: String -> Disc
parseDisc s = Disc name weight stack
  where
    name = takeWhile (/= ' ') s
    weight = read $ takeWhile (/= ' ') $ drop (1 + (length name)) s
    stack = filter (/= "") $ splitOn ", " $ drop 2 $ dropWhile (/= '>') s

getBottom :: [Disc] -> String
getBottom ds = head $ (map name ds) L.\\ (nub $ concat $ map stack ds)
