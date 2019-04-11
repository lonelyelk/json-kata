module JSON (JSON(..), parseJSON) where

import Text.Parsec
import Text.Parsec.String

data JSON = JSONString String deriving (Show, Eq)

parseJSON :: String -> Either ParseError JSON
parseJSON str = parse jsonVal "json" str

jsonVal :: Parser JSON
jsonVal = JSONString <$> between (char '"') (char '"') (many (noneOf "\"\\"))


data Assertion a = Pass | Fail a deriving (Show, Eq)

assertEqual :: (Eq b) => a -> b -> b -> Assertion a
assertEqual msg val1 val2
    | val1 == val2 = Pass
    | otherwise = Fail msg

testParse :: [Assertion String]
testParse =
    foldr (\a acc -> if a == Pass then acc else a:acc) [] [
        assertEqual "Expected \"\" to yield empty string" (parseJSON "\"\"") (Right (JSONString "")),
        assertEqual "Expected \"abc\" to yield a string" (parseJSON "\"abc\"") (Right (JSONString "abc"))
    ]
