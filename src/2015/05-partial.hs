import Data.List (isInfixOf)

main :: IO ()
main = interact (formatOutput . solve2 . parseInput)

formatOutput :: Int -> String
formatOutput solution = show solution ++ "\n"

parseInput :: String -> [String]
parseInput rawInput = lines rawInput

solve1 :: [String] -> Int
solve1 s = length $ filter niceString1 s

niceString1 :: String -> Bool
niceString1 s = atLeastThreeVowels s && atLeastOneLetterAppearingTwice s && noBadStrings s

atLeastThreeVowels :: String -> Bool
atLeastThreeVowels s = length vowels >= 3
  where vowels = filter isVowel s

atLeastOneLetterAppearingTwice :: String -> Bool
atLeastOneLetterAppearingTwice (c:c':s) = if c == c' then True else atLeastOneLetterAppearingTwice (c':s)
atLeastOneLetterAppearingTwice _ = False

noBadStrings :: String -> Bool
noBadStrings s = not ("ab" `isInfixOf` s || "cd" `isInfixOf` s || "pq" `isInfixOf` s || "xy" `isInfixOf` s)

isVowel :: Char -> Bool
isVowel 'a' = True
isVowel 'i' = True
isVowel 'u' = True
isVowel 'e' = True
isVowel 'o' = True
isVowel _ = False


solve2 :: [String] -> Int
solve2 s = length $ filter niceString2 s

niceString2 :: String -> Bool
niceString2 s = twoPair s && repeatedCharacter s

-- TODO: Make it so that this returns False for "aaa", since the pairs can't overlap
twoPair :: String -> Bool
twoPair s =
  let
    pairs      = zip s (drop 1 s)
    pairCounts = map (\pair -> numTimes pair pairs) pairs
  in
    any (>= 2) pairCounts

numTimes :: Eq a => a -> [a] -> Int
numTimes x xs = (length . filter (== x)) xs


repeatedCharacter :: String -> Bool
repeatedCharacter (a:b:a':rest) = if a == a' then True else repeatedCharacter (b:a':rest)
repeatedCharacter _ = False
