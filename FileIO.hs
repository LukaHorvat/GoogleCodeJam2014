module FileIO where

import Text.Parsec.String
import Text.Parsec
import Control.Applicative ((<*), liftA2, Applicative)

withSpaces :: Parser a -> Parser a
withSpaces parser = parser <* skipMany (space <|> newline)

readCases :: String -> Parser a -> IO [a]
readCases path caseParser = do
    e <- parseFromFile parser path
    case e of
        Left err -> error $ show err
        Right a  -> return a
    where parser = do
            n <- withSpaces $ fmap read $ many1 digit :: Parser Int
            many1 caseParser

prefixSolutions :: [String] -> [String]
prefixSolutions = zipWith (\n s -> "Case #" ++ show n ++ ": " ++ s) [1..]

printSolutions :: [String] -> IO ()
printSolutions = putStrLn . unlines . prefixSolutions

writeSolutions :: String -> [String] -> IO ()
writeSolutions path = writeFile path . unlines . prefixSolutions

number :: Parser Int
number = withSpaces $ fmap read $ many1 digit

num :: (Num a, Read a) => Parser a
num = withSpaces $ fmap read $ many1 digit

(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
(<++>) = liftA2 (++)

double :: Parser Double
double = withSpaces $ fmap read $ do
    whole <- many1 digit
    maybeDec <- optionMaybe $ string "." <++> many1 digit
    case maybeDec of Just dec -> return $ whole ++ dec
                     Nothing  -> return whole
