import Data.Hash.MD5

main :: IO ()
main = interact (formatOutput . solve2 . parseInput)

formatOutput :: Int -> String
formatOutput solution = show solution ++ "\n"

parseInput :: String -> String
parseInput rawInput = head $ lines rawInput

solve1 :: String -> Int
solve1 s = go s 0

go :: String -> Int -> Int
go s number =
  let
    x    = s ++ show number
    hash = md5s (Str x)
  in
    if checkHash hash
      then number
      else go s (number + 1)

checkHash :: String -> Bool
checkHash hash = "00000" == take 5 hash

solve2 :: String -> Int
solve2 s = go2 s 0

go2 :: String -> Int -> Int
go2 s number =
  let
    x    = s ++ show number
    hash = md5s (Str x)
  in
    if checkHash2 hash
      then number
      else go2 s (number + 1)

checkHash2 :: String -> Bool
checkHash2 hash = "000000" == take 6 hash
