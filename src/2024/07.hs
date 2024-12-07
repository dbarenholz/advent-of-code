parse :: [String] -> [(Int, [Int])]
parse = map $ \line ->
  (read $ takeWhile (/= ':') line
  ,map read (words $ drop 2 $ dropWhile (/= ':') line))

opLists :: Int -> [[Char]]
opLists 0 = [""]
opLists n = concatMap (\lst -> ['*':lst, '+':lst, '|':lst]) (opLists (n-1))

solve :: [(Int, [Int])] -> Int
solve ((val, nums) : lst) =
  let ops       = opLists (length nums)
      evaluated = map (eval nums) ops
      equal     = val `elem` evaluated
  in if equal then val + solve lst else solve lst
solve [] = 0

eval :: [Int] -> [Char] -> Int
eval (n:nums) ops = go n nums ops
  where go acc (n:nums) ('+':ops) = go (acc + n) nums ops
        go acc (n:nums) ('*':ops) = go (acc * n) nums ops
        go acc (n:nums) ('|':ops) = go (read $ show acc ++ show n) nums ops
        go acc _ _ = acc

main :: IO ()
main = interact (show . solve . parse . lines)
