import Data.List (intercalate, findIndices, isPrefixOf, tails, unfoldr, nub, permutations)
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import qualified Data.List.NonEmpty as NE

import Data.Set (Set)
import qualified Data.Set as Set

main :: IO ()
main = interact (formatOutput . solve2 . parseInput)

-- | Returns list of strings that are known to be non empty, obtained by splitting `s` on `sep`.
splitOn' :: Char -> String -> NonEmpty String
splitOn' sep s = case s of
  []                 -> "" :| [];
  (part : remainder) -> if part == sep
    then "" <| splitOn' sep remainder
    else let head :| tail = splitOn' sep remainder in (part : head) :| tail

-- | Returns list of strings obtained by splitting `s` on `sep`.
splitOn :: Char -> String -> [String]
splitOn sep s = NE.toList (splitOn' sep s)

-- | Infix operation for splitting strings
(-|-) :: String -> Char -> [String]
s -|- sep = splitOn sep s

-- | Returns starting indices of `needle` in `s`. Returns the empty list if not found.
findString :: String -> String -> [Int]
findString needle s = findIndices (isPrefixOf needle) (tails s)

-- | List indexing that returns Nothing if index is out of bounds.
--  Otherwise, return `Just (s !! i)`
atIndex :: [a] -> Int -> Maybe a
atIndex s i = if 0 <= i && i < length s then Just (s !! i) else Nothing

-- | Infix operator for indexing 1D arrays to `Maybe`s.
(!!?) :: [a] -> Int -> Maybe a
s !!? i = atIndex s i

-- | List indexing on 2D lists.
-- Returns Nothing if (either) index is out of bounds.
-- Otherwise, return `Just (mat !! j) !! i`
atIndex2D :: [[a]] -> Int -> Int -> Maybe a
atIndex2D mat i j = case atIndex mat j of
  Nothing -> Nothing
  Just x  -> x !!? i

-- Define input and output here
type Node = String
type Edge = (Node, Node, Int)
type Graph = [Edge]

type Input  = Graph
type Output = Int

formatOutput :: Output -> String
formatOutput solution = show solution ++ "\n"

parseInput :: String -> Input
parseInput rawInput = concatMap (\line ->
  let ws     = words line
      from   = head ws
      to     = ws !! 2
      weight = read (ws !! 4)
      -- NOTE: add both directions to the list
  in [(from, to, weight), (to, from, weight)]) (lines rawInput)

nodesFrom :: Graph -> [Node]
nodesFrom g = nub $ map (\(from, _, _) -> from) g

processPerm :: Graph -> [Node] -> Int
processPerm g (from:to:rest) = distanceFor g from to + processPerm g (to:rest)
processPerm g _ = 0

distanceFor :: Graph -> Node -> Node -> Int
distanceFor ((from', to', w) : es) from to | from' == from && to' == to = w
distanceFor ((from', to', w) : es) from to | otherwise                  = distanceFor es from to
distanceFor [] _ _ = 0

solve1 :: Input -> Output
solve1 g = minimum $ map (processPerm g) (permutations $ nodesFrom g)

solve2 :: Input -> Output
solve2 g = maximum $ map (processPerm g) (permutations $ nodesFrom g)
