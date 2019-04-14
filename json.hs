module JSON (JSON(..), parseJSON) where

import Text.Parsec
import Text.Parsec.String

data JSON = JSONString String | JSONBool Bool deriving (Show, Eq)

parseJSON :: String -> Either ParseError JSON
parseJSON = parse jsonVal "json"

jsonVal :: Parser JSON
jsonVal = choice [jsonString, jsonBool]

jsonString :: Parser JSON
jsonString = JSONString <$> between (char '"') (char '"') (many (noneOf "\"\\"))

jsonBool :: Parser JSON
jsonBool = (JSONBool True <$ string "true") <|> (JSONBool False <$ string "false")



data Assertion a = Pass | Fail a deriving (Show, Eq)

assertEqual :: (Eq b) => a -> b -> b -> Assertion a
assertEqual msg val1 val2
    | val1 == val2 = Pass
    | otherwise = Fail msg

testParse :: [Assertion String]
testParse =
    foldr (\a acc -> if a == Pass then acc else a:acc) [] [
        assertEqual "Expected \"\" to yield empty string" (parseJSON "\"\"") (Right (JSONString "")),
        assertEqual "Expected \"abc\" to yield a string" (parseJSON "\"abc\"") (Right (JSONString "abc")),
        assertEqual "Expected true to yield a boolean" (parseJSON "true") (Right (JSONBool True)),
        assertEqual "Expected false to yield a boolean" (parseJSON "false") (Right (JSONBool False))
    ]
