import FileIO
import Text.Parsec
import Text.Parsec.String
import Control.Applicative
import Data.List
import Control.Monad
import Data.Char  (digitToInt)
import Numeric    (readInt)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Bits

readBin :: Integral a => String -> a
readBin = fst . head . readInt 2 (`elem` "01") digitToInt

data Case = Case [Integer] [Integer] deriving Show

caseParser :: Parser Case
caseParser = do
    n <- number
    number
    [outs, devices] <- count 2 $ count n $ withSpaces $ many1 digit
    return $ Case (map readBin outs) (map readBin devices)

caseSolver :: Case -> String
caseSolver (Case outs devices)
    | Set.null goodMoves = "NOT POSSIBLE"
    | otherwise          = show $ Set.findMin $ Set.map popCount goodMoves
    where sets = Set.fromList <$> (map (\d -> map (xor d) outs) devices)
          goodMoves = foldl1 Set.intersection sets

solution :: IO ()
solution = do
    cases <- readCases "A-large-practice.in" caseParser
    let solutions = map caseSolver cases
    writeSolutions "out.txt" solutions
