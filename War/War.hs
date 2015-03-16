import FileIO
import Text.Parsec
import Text.Parsec.String
import Control.Applicative
import Control.Monad
import qualified Data.List as List
import qualified Data.Sequence as Seq

data Case = Case [Double] [Double] deriving Show

caseParser :: Parser Case
caseParser = do
    n <- number
    Case <$> count n double <*> count n double

splitWhile :: (a -> Bool) -> [a] -> ([a], [a])
splitWhile f (x : xs) | f x       = (x : yes, no)
                      | otherwise = ([], x : xs)
                      where (yes, no) = splitWhile f xs
splitWhile _ [] = ([], [])

--His optimal strategy is to play the lightest block that still beats her block.
--If such a block doesn't exist, he plays the lightest block he has (to cut his losses)
--Her best bet is to play the heaviest block she has (probably). That way, if she has any chance
--of winning, he'll have to discard some of his lighter block which wins her a few more points at
--the end.
caseSolver :: Case -> (Int, Int)
caseSolver (Case her his) = (best, def)
    where herSort = reverse $ List.sort her
          hisSort = reverse $ List.sort his
          defaultOutcome [] [] res = res
          defaultOutcome (x : xs) (y : ys) (s1, s2)
              | x > y     = defaultOutcome xs (init $ y : ys) (s1 + 1, s2)
              | otherwise = defaultOutcome xs removedBest (s1, s2 + 1)
              where (yes, no) = splitWhile (> x) (y : ys)
                    removedBest = init yes ++ no
          (best, _) = optimal her his
          (def, _) = defaultOutcome herSort hisSort (0, 0)

--Her optimal cheating strategy is to play the lightest block that's heavier than his lightest block.
--She then says a number greater than any of his blocks, making him play the lightest one he has.
--If his lightest block if heacier than all of her blocks, she can't win any more points.
optimal :: [Double] -> [Double] -> (Int, Int)
optimal her his = (length her - hisScore, hisScore)
    where hisScore = optimal' herSort hisSort
          herSort = List.sort her
          hisSort = List.sort his
          optimal' [] []       = 0
          optimal' xs (y : ys) = case no of
              []         -> length (y : ys)
              (_ : rest) -> optimal' (yes ++ rest) ys
              where (yes, no) = splitWhile (< y) xs

solution :: IO ()
solution = do
    cases <- readCases "D-large-practice.in" caseParser
    let solutions = map ((\(x, y) -> show x ++ " " ++ show y) . caseSolver) cases
    writeSolutions "out.txt" solutions
