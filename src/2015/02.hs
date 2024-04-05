import Data.List (intercalate, sort)
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

type Box = (Int, Int, Int)

parseInput :: String -> [Box]
parseInput rawInput = map processLine (lines rawInput)
  where
    processLine line =
      let
        tmp = map read (splitOn 'x' line)
      in
        (tmp !! 0, tmp !! 1, tmp !! 2)

solve1 :: [Box] -> Int
solve1 boxes = sum $ map getAmount boxes
  where
    getAmount box = surfaceOf box + slackOf box

-- Compute the surface area in "feet squared"
surfaceOf :: Box -> Int
surfaceOf (l, w, h) = 2 * h * l + 2 * l * w + 2 * w * h

-- Compute slack - area of smallest side
slackOf :: Box -> Int
slackOf (l, w, h) =
  let
    sorted = sort [l, w, h]
  in
    sorted !! 0 * sorted !! 1

solve2 :: [Box] -> Int
solve2 boxes = sum $ map getAmount boxes
  where
    getAmount box = wrapFor box + bowFor box

wrapFor :: Box -> Int
wrapFor (l, w, h) =
  let
    sorted = sort [l, w, h]
    a      = sorted !! 0
    b      = sorted !! 1
  in
    a + a + b + b

bowFor :: Box -> Int
bowFor (l, w, h) = l * w * h


