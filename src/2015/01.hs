import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import qualified Data.List.NonEmpty as NE

main :: IO ()
main = interact (formatOutput . solve2 . parseInput)

-- helpers for parsing
splitOn' :: Char -> String -> NonEmpty String
splitOn' sep s = case s of { [] -> "" :| []; (part : remainder) -> if part == sep then "" <| splitOn' sep remainder else let head :| tail = splitOn' sep remainder in (part : head) :| tail}

splitOn :: Char -> String -> [String]
splitOn sep s = NE.toList (splitOn' sep s)

formatOutput :: Int -> String
formatOutput solution = show solution ++ "\n"

parseInput :: String -> String
parseInput rawInput = head $ lines rawInput

-- Idea: replace each '(' with 1 and each ')' with -1, then sum the list
solve1 :: String -> Int
solve1 s = sum $ map replaceChar s

replaceChar :: Char -> Int
replaceChar '(' = 1
replaceChar ')' = -1
replaceChar _ = 0

-- Need to keep track of the first occurence something happens
-- So we have to "loop"
solve2 :: String -> Int
solve2 s = go 0 (map replaceChar s) 0

go :: Int -> [Int] -> Int -> Int
go i _ (-1) = i
go i (x:xs) sum = go (i + 1) xs (sum + x)
go _ [] _ = -1
