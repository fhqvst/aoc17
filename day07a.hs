import Data.List as L
import Data.List.Split
import qualified Data.Map as Map
import Data.Map as M hiding (map, filter)

data Disc = Disc { name :: String, weight :: Int, stack :: [String] }
  deriving Show

main :: IO ()
main = do
  discs <- map parseDisc <$> lines <$> readFile "input"
  print $ head $ (map name discs) L.\\ (nub $ concat $ map stack discs)

parseDisc :: String -> Disc
parseDisc s = Disc name weight stack
  where
    name = takeWhile (/= ' ') s
    weight = read $ takeWhile (/= ' ') $ drop (1 + (length name)) s
    stack = filter (/= "") $ splitOn ", " $ drop 2 $ dropWhile (/= '>') s
