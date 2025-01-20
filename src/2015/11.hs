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


hasSequence :: String -> Bool
hasSequence (c1:c2:c3:rest) = isIncreasing c1 c2 c3 || hasSequence (c2:c3:rest)
hasSequence _ = False

-- | zab or yza are _not_ increasing, so explicitly set to false
isIncreasing :: Char -> Char -> Char -> Bool
isIncreasing a b c | a == 'z' = False
isIncreasing a b c | b == 'z' = False
isIncreasing a b c = next a == b && next b == c

hasValidCharacters :: String -> Bool
hasValidCharacters s = not ('i' `elem` s || 'o' `elem` s || 'l' `elem` s)

hasTwoNonOverlappingPairs :: String -> Bool
hasTwoNonOverlappingPairs s = 2 <= countPairs [] 0 (zip s [0..])

-- countPairs [] 0 pairs
countPairs :: [Int] -> Int -> [(Char, Int)] -> Int
countPairs used count ((c1, i1) :   (c2, i2) : rest) | c1 == c2 && not (i1 `elem` used || i2 `elem` used) = countPairs (i1:i2:used) (count + 1) rest
countPairs used count ((c1, i1) : t@(c2, i2) : rest) | otherwise                                          = countPairs used count (t:rest)
countPairs used count _  = count

increment :: String -> String
increment ('z':'z' :'z':'z':'z':'z':'z':'z' :_) = "DONE"
increment (c1 :'z' :'z':'z':'z':'z':'z':'z':_) = [next c1, 'a',     'a',     'a',     'a',     'a',     'a',     'a']
increment (c1 : c2 :'z':'z':'z':'z':'z':'z':_) = [c1,      next c2, 'a',     'a',     'a',     'a',     'a',     'a']
increment (c1 : c2 :c3 :'z':'z':'z':'z':'z':_) = [c1,      c2,      next c3, 'a',     'a',     'a',     'a',     'a']
increment (c1 : c2 :c3 :c4 :'z':'z':'z':'z':_) = [c1,      c2,      c3,      next c4, 'a',     'a',     'a',     'a']
increment (c1 : c2 :c3 :c4 :c5 :'z':'z':'z':_) = [c1,      c2,      c3,      c4,      next c5, 'a',     'a',     'a']
increment (c1 : c2 :c3 :c4 :c5 :c6 :'z':'z':_) = [c1,      c2,      c3,      c4,      c5,      next c6, 'a',     'a']
increment (c1 : c2 :c3 :c4 :c5 :c6 :c7 :'z':_) = [c1,      c2,      c3,      c4,      c5,      c6,      next c7, 'a']
increment (c1 : c2 :c3 :c4 :c5 :c6 :c7 :c8 :_) = [c1,      c2,      c3,      c4,      c5,      c6,      c7,      next c8]

next :: Char -> Char
next 'a' = 'b'
next 'b' = 'c'
next 'c' = 'd'
next 'd' = 'e'
next 'e' = 'f'
next 'f' = 'g'
next 'g' = 'h'
next 'h' = 'i'
next 'i' = 'j'
next 'j' = 'k'
next 'k' = 'l'
next 'l' = 'm'
next 'm' = 'n'
next 'n' = 'o'
next 'o' = 'p'
next 'p' = 'q'
next 'q' = 'r'
next 'r' = 's'
next 's' = 't'
next 't' = 'u'
next 'u' = 'v'
next 'v' = 'w'
next 'w' = 'x'
next 'x' = 'y'
next 'y' = 'z'
next 'z' = 'a'

-- INFO: add puzzle input in here, removed for github
input1 :: String
input1 = "aaaaaaaa"

solve1 :: String
solve1 = loop input1
  where loop :: String -> String
        loop s | hasSequence s && hasValidCharacters s && hasTwoNonOverlappingPairs s = s
        loop s = loop (increment s)

-- INFO: add answer to part 1 as input here, removed for github
input2 :: String
input2 = increment "aaaaaaaa"

solve2 :: String
solve2 = loop input2
  where loop :: String -> String
        loop s | hasSequence s && hasValidCharacters s && hasTwoNonOverlappingPairs s = s
        loop s = loop (increment s)
