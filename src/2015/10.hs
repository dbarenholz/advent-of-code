import Data.List (intercalate, findIndices, isPrefixOf, tails, group)
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import qualified Data.List.NonEmpty as NE

main :: IO ()
main = print solve2

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

input :: String
input = "1113222113"

solve1 :: Int
solve1 = length $ go 40 input
  where go :: Int -> String -> String
        go n s | n > 0 = go (n - 1) $ concatMap (\l -> show (length l) ++ [head l]) (group s)
        go n s | otherwise = s

solve2 = length $ go 50 input
  where go :: Int -> String -> String
        go n s | n > 0 = go (n - 1) $ concatMap (\l -> show (length l) ++ [head l]) (group s)
        go n s | otherwise = s
