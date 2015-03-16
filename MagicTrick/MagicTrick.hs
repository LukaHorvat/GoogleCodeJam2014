import FileIO
import Text.Parsec
import Text.Parsec.String
import Control.Applicative hiding ((<|>))
import Control.Monad
import Data.List

data Case = Case Int [[Int]] Int [[Int]] deriving Show

parseCase :: Parser Case
parseCase = withSpaces $
    Case <$> number <*> count 4 (count 4 number) <*> number <*> count 4 (count 4 number)

solution :: IO ()
solution = do
    cases <- readCases "A-small-practice.in" parseCase
    printSolutions $ map solveCase cases

solveCase :: Case -> String
solveCase (Case n1 arr1 n2 arr2) = case (arr1 !! (n1 - 1)) `intersect` (arr2 !! (n2 - 1)) of
    []  -> "Volunteer cheated!"
    [x] -> show x
    _   -> "Bad magician!"
