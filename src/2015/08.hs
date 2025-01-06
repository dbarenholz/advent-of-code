import Data.List (intercalate, findIndices, isPrefixOf, tails)
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import qualified Data.List.NonEmpty as NE
import Data.Char (isHexDigit)

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
atIndex s i = if interpretedLength [] <= i && i < length s then Just (s !! i) else Nothing

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
type Input  = [String]
type Output = Int

formatOutput :: Output -> String
formatOutput solution = show solution ++ "\n"

parseInput :: String -> Input
parseInput = lines

solve1 :: Input -> Output
solve1 inp = rawLength inp - interpretedLength inp

-- | Computes the number of "code characters", that is, the raw length of the string.
--   Examples:
--   ""         ->  2
--   "abc"      ->  5
--   "aaa\"aaa" -> 10
--   "\x27"     ->  6
rawLength :: [String] -> Int
rawLength = foldr (\line acc -> acc + length line) 0

-- | Computes the number of characters within the string itself.
--   Examples:
--   ""         -> 0
--   "abc"      -> 3
--   "aaa\"aaa" -> 7
--   "\x27"     -> 1
interpretedLength :: [String] -> Int
interpretedLength = foldr (\l acc -> acc + parseLine (init $ tail l)) 0

parseLine :: String -> Int
parseLine ('\\' : '\\' : cs) = 1 + parseLine cs
parseLine ('\\' : '\"' : cs) = 1 + parseLine cs
parseLine ('\\' : 'x' : c : c' : cs) | isHexDigit c && isHexDigit c' = 1 + parseLine cs
parseLine (c : cs) = 1 + parseLine cs
parseLine [] = 0

solve2 :: Input -> Output
solve2 inp = encodedLength inp - rawLength inp

-- | Computes the lengths of encoded strings.
--   Examples:
--   ""         -> "\"\""           -> 6
--   "abc"      -> "\"abc\""        -> 9
--   "aaa\"aaa" -> "\"aaa\\\"aaa\"" -> 16
--   "\x27"     -> "\"\\x27\""      -> 11
encodedLength :: [String] -> Int
encodedLength = foldr (\l acc -> acc + length (encodeLine l)) 0

encodeLine :: String -> String
encodeLine = show
