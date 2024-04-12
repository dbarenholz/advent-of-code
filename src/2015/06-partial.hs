import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import qualified Data.List.NonEmpty as NE

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.List as List

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

main :: IO ()
main = interact (formatOutput . solve2 . parseInput)

splitOn' :: Char -> String -> NonEmpty String
splitOn' sep s = case s of { [] -> "" :| []; (part : remainder) -> if part == sep then "" <| splitOn' sep remainder else let head :| tail = splitOn' sep remainder in (part : head) :| tail}

splitOn :: Char -> String -> [String]
splitOn sep s = NE.toList (splitOn' sep s)

formatOutput :: Int -> String
formatOutput solution = show solution ++ "\n"

type Position = (Int, Int)
data Action   = ON | OFF | TOGGLE deriving Show
type Input    = (Action, Position, Position)

parseInput :: String -> [Input]
parseInput rawInput = map parseAction (lines rawInput)

parseAction :: String -> Input
parseAction s =
  let w = words s
      action  = w !! 0
      action' = w !! 1
  in  if action == "toggle"
      then getAction TOGGLE (drop 1 w)
      else case action' of
        "on"  -> getAction ON  (drop 2 w)
        "off" -> getAction OFF (drop 2 w)
        tmp -> error $ "unexpected action '" ++ tmp ++ "'"

getAction :: Action -> [String] -> Input
getAction act w =
  let p1 = parsePosition $ head w
      p2 = parsePosition $ last w
  in  (act, p1, p2)

-- "aaaa,bbbb" --> (aaaa, bbbb)
parsePosition :: String -> Position
parsePosition s =
  let nums = splitOn ',' s
      a = read $ head nums :: Int
      b = read $ last nums :: Int
  in  (a, b)

solve1 :: [Input] -> Int
solve1 acts = Set.size $ List.foldl' processAction Set.empty acts

processAction :: Set Position -> Input -> Set Position
processAction s (action, from, to) =
  let pos = Set.fromList $ positions from to
  in  case action of
    ON     -> Set.union      s pos
    OFF    -> Set.difference s pos
    TOGGLE -> Set.foldl' (\set item -> if Set.member item set then Set.delete item set else Set.insert item set) s pos

positions :: Position -> Position -> [Position]
positions (from_x, from_y) (to_x, to_y) =
  let x_rng = [from_x .. to_x]
      y_rng = [from_y .. to_y]
      getPositions (x:xs) ys = (map (\y -> (x, y)) ys) ++ getPositions xs ys
      getPositions _      _  = []
  in  getPositions x_rng y_rng

-- WRONG: cannot do this, because OFF now subtracts (potentially) from lamps it should not be subtracting from.
-- ANS: 14210482
-- solve2 :: [Input] -> Int
-- solve2 acts = List.foldl' go 0 acts
--
-- go :: Int -> Input -> Int
-- go count (action, from, to) =
--   let n = length $ positions from to
--   in  case action of
--     ON     -> count + n
--     OFF    -> if count - n < 0 then 0 else count - n
--     TOGGLE -> count + (2 * n)

-- 14914565; also wrong, I don't get it
solve2 :: [Input] -> Int
solve2 acts = sum $ Map.elems $ List.foldl' go Map.empty acts

go :: Map Position Int -> Input -> Map Position Int
go mp (action, from, to) =
  let pos = positions from to
  in  case action of
    ON     -> List.foldl' (\mp' key -> Map.insertWith (+) key 1 mp') mp pos
    TOGGLE -> List.foldl' (\mp' key -> Map.insertWith (+) key 2 mp') mp pos
    OFF    -> List.foldl' (\mp' key -> Map.insertWith (\one val -> maximum [0, val - one]) key 1 mp') mp pos
