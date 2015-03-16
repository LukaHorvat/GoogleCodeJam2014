import FileIO
import Text.Parsec
import Text.Parsec.String
import Control.Applicative

data Case = Case Double Double Double

parseCase :: Parser Case
parseCase = withSpaces $ Case <$> double <*> double <*> double

solution :: IO ()
solution = do
    cases <- readCases "B-large-practice.in" parseCase
    writeSolutions "out.txt" $ map solveCase cases

solveCase :: Case -> String
solveCase (Case c f x) = show $ select $ map (\(_, x, y) -> x + y) $ iterate buy (2.0, 0, x / 2.0)
    where buy (rate, time, _) = (rate + f, c / rate + time, x / (rate + f))
          select (x : y : xs) | y > x     = x
                              | otherwise = select $ y : xs
