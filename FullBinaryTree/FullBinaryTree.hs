import FileIO
import Text.Parsec
import Text.Parsec.String
import Control.Applicative
import Data.List
import Data.Ord
import Data.Function
import Control.Monad
import Data.Map (Map)
import Data.MemoTrie
import qualified Data.Map as Map

data Case = Case (Map Int [Int]) deriving Show

caseParser :: Parser Case
caseParser = do
    n <- number
    edges <- count (n - 1) $ (,) <$> number <*> number
    let fullyConnected = concatMap (\(x, y) -> [(x, y), (y, x)]) edges
        sorted = sort fullyConnected
        grouped = groupBy ((==) `on` fst) sorted
    return $ Case $ Map.fromList $ map (\xs@((x, y) : _) -> (x, map snd xs)) grouped

countFrom :: Map Int [Int] -> (Int, Int) -> Int
countFrom tree = countFrom'
    where countFrom' = memo countFrom''
          countFrom'' (from, to) = 1 + sum (map countFrom' $ zip [to, to..] children)
              where children = filter (/= from) $ tree Map.! to

minPrune :: Map Int [Int] -> (Int, Int) -> Int
minPrune tree = minPrune'
    where count = countFrom tree
          minPrune' = memo minPrune''
          minPrune'' (from, to)
              | numChild == 0 = 0
              | numChild == 1 = head subtrees
              | numChild == 2 = sum prunes
              | numChild > 2  = minimum $ map (\((_, cut1, prune1), (_, cut2, prune2)) ->
                  cutAll - cut1 - cut2 + prune1 + prune2) choose2
              where children = filter (/= from) $ tree Map.! to
                    numChild = length $ children
                    pairs    = zip [to, to..] children
                    subtrees = map count pairs
                    prunes   = map minPrune' pairs
                    triples  = zip3 [1..] subtrees prunes
                    choose2  = filter (\(x, y) -> x /= y) $ (,) <$> triples <*> triples
                    cutAll   = sum subtrees

caseSolver :: Case -> Int
caseSolver (Case m) = minimum $ map (minPrune m) $ zip [0, 0..] $ Map.keys m

solution :: IO ()
solution = do
    cases <- readCases "B-large-practice.in" caseParser
    let solutions = map (show . caseSolver) cases
    writeSolutions "out.txt" solutions
