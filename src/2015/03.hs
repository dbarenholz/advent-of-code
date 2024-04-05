import Data.List (intercalate, sort)
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import qualified Data.List.NonEmpty as NE

import Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main = interact (formatOutput . solve2 . parseInput)

-- helpers for parsing
splitOn' :: Char -> String -> NonEmpty String
splitOn' sep s = case s of { [] -> "" :| []; (part : remainder) -> if part == sep then "" <| splitOn' sep remainder else let head :| tail = splitOn' sep remainder in (part : head) :| tail}

splitOn :: Char -> String -> [String]
splitOn sep s = NE.toList (splitOn' sep s)

formatOutput :: Int -> String
formatOutput solution = show solution ++ "\n"

type Pos = (Int, Int)

parseInput :: String -> String
parseInput rawInput = head $ lines rawInput

start :: Pos
start = (0, 0)

mp :: Map Pos Int
mp = Map.fromList [(start, 1)]

solve1 :: String -> Int
solve1 s = go s start mp

go :: String -> Pos -> Map Pos Int -> Int
go [] _ mp = Map.size mp
go ('^':xs) (x, y) mp =
  let newPos = (x, y + 1)
      newMap = Map.insertWith (+) newPos 1 mp
  in  go xs newPos newMap
go ('v':xs) (x, y) mp =
  let newPos = (x, y - 1)
      newMap = Map.insertWith (+) newPos 1 mp
  in  go xs newPos newMap
go ('<':xs) (x, y) mp =
  let newPos = (x - 1, y)
      newMap = Map.insertWith (+) newPos 1 mp
  in  go xs newPos newMap
go ('>':xs) (x, y) mp =
  let newPos = (x + 1, y)
      newMap = Map.insertWith (+) newPos 1 mp
  in  go xs newPos newMap

solve2 :: String -> Int
solve2 s = go2 inp start start mp
  where
    inp = zip s (cycle ["meat", "robo"])

go2 :: [(Char, String)] -> Pos -> Pos -> Map Pos Int -> Int
go2 [] _ _ mp = Map.size mp
go2 ((dir, "meat"):xs) (x, y) roboPos mp = case dir of
  '^' -> let newPos = (x + 1, y)
             newMap = Map.insertWith (+) newPos 1 mp
         in  go2 xs newPos roboPos newMap
  'v' -> let newPos = (x - 1, y)
             newMap = Map.insertWith (+) newPos 1 mp
         in  go2 xs newPos roboPos newMap
  '<' -> let newPos = (x, y - 1)
             newMap = Map.insertWith (+) newPos 1 mp
         in  go2 xs newPos roboPos newMap
  '>' -> let newPos = (x, y + 1)
             newMap = Map.insertWith (+) newPos 1 mp
         in  go2 xs newPos roboPos newMap
go2 ((dir, "robo"):xs) meatPos (x, y) mp = case dir of
  '^' -> let newPos = (x + 1, y)
             newMap = Map.insertWith (+) newPos 1 mp
         in  go2 xs meatPos newPos newMap
  'v' -> let newPos = (x - 1, y)
             newMap = Map.insertWith (+) newPos 1 mp
         in  go2 xs meatPos newPos newMap
  '<' -> let newPos = (x, y - 1)
             newMap = Map.insertWith (+) newPos 1 mp
         in  go2 xs meatPos newPos newMap
  '>' -> let newPos = (x, y + 1)
             newMap = Map.insertWith (+) newPos 1 mp
         in  go2 xs meatPos newPos newMap
