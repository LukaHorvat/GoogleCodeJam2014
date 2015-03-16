import FileIO
import Text.Parsec
import Text.Parsec.String
import Control.Applicative
import Data.List
import Control.Monad

data Case = Case Int Int Int

caseParser :: Parser Case
caseParser = withSpaces $ Case <$> number <*> number <*> number

trivial :: Int -> Int -> [[Char]]
trivial rows cols = ('c' : replicate (cols - 1) '*') : replicate (rows - 1) (replicate cols '*')

placeClick :: [[Char]] -> [[Char]]
placeClick ((_ : xs) : xss) = ('c' : xs) : xss

toMap :: Int -> [Char] -> [[Char]]
toMap _ [] = []
toMap n xs = take n xs : toMap n (drop n xs)

--No pretty way of explaining this.
--A ton of special cases and map generation boilerplate
solveCase :: Case -> Maybe [[Char]]
solveCase (Case rows cols n) | rows > cols = transpose <$> (solveCase $ Case cols rows n)
solveCase (Case 1 cols n) = Just $ placeClick $ return $ replicate (cols - n) '.' ++ replicate n '*'
solveCase (Case 2 cols n)
    | n == 2 * cols - 1       = Just $ trivial 2 cols
    | free >= 4 && even free  = Just $ placeClick $ replicate 2 $
                                    replicate (free `div` 2) '.' ++ replicate (n `div` 2) '*'
    | otherwise               = Nothing
    where free = 2 * cols - n
solveCase (Case rows cols n)
    | free `elem` [2, 3, 5, 7]         = Nothing
    | free == 1                        = Just $ trivial rows cols
    | even free && free <= 2 * cols    = Just $ placeClick $
        (replicate 2 $ replicate (free `div` 2) '.' ++ replicate (cols - free `div` 2) '*') ++
        (replicate (rows - 2) $ replicate cols '*')
    | odd free && free <= 2 * cols + 3 = Just $ placeClick $
        (replicate 2 $ replicate (free `div` 2 - 1) '.' ++ replicate (cols - free `div` 2 + 1) '*') ++
        ["..." ++ replicate (cols - 3) '*'] ++
        (replicate (rows - 3) $ replicate cols '*')
    | free `mod` cols /= 1             = Just $ placeClick $
        toMap cols $ (replicate free '.' ++ replicate n '*')
    | otherwise                        = Just $ placeClick $
        (replicate (fullRows - 1) $ replicate cols '.') ++
        [replicate (cols - 1) '.' ++ "*"] ++
        [replicate (rest + 1) '.' ++ replicate (cols - rest - 1) '*'] ++
        (replicate (rows - fullRows - 1) $ replicate cols '*')
    where free = rows * cols - n
          (fullRows, rest) = free `quotRem` cols

solution :: IO ()
solution = do
    cases <- readCases "C-large-practice.in" caseParser
    let solutions = map solveCase cases
    writeFile "out.txt" $ concat $ map out $ zip [1..] solutions
    where out (i, sol) = "Case #" ++ show i ++ ":\n" ++
              case sol of
                  Nothing -> "Impossible\n"
                  Just m  -> unlines m
