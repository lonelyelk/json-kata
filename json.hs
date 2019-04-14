module JSON (JSON(..), parseJSON) where

import Text.Parsec
import Text.Parsec.String
import Control.Monad (void)

data JSON = JSONString String | JSONBool Bool | JSONNull | JSONArray [JSON] deriving (Show, Eq)

parseJSON :: String -> Either ParseError JSON
parseJSON = parse (whiteSpace *> jsonVal) "json"

jsonVal :: Parser JSON
jsonVal = choice [jsonString, jsonBool, jsonNull, jsonArray]

jsonString :: Parser JSON
jsonString = JSONString <$> (between (char '"') (char '"') (many (noneOf "\"\\")) <* whiteSpace)

jsonBool :: Parser JSON
jsonBool = (JSONBool True <$ (string "true" <* whiteSpace)) <|> (JSONBool False <$ (string "false" <* whiteSpace))

jsonNull :: Parser JSON
jsonNull = JSONNull <$ (string "null" <* whiteSpace)

jsonArray :: Parser JSON
jsonArray = JSONArray <$> (between (char '[' *> whiteSpace) (char ']' *> whiteSpace) (jsonVal `sepBy` (char ',' *> whiteSpace)) <* whiteSpace)

whiteSpace :: Parser ()
whiteSpace = void $ many $ oneOf " \n\t\r"


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
        assertEqual "Expected false to yield a boolean" (parseJSON "false") (Right (JSONBool False)),
        assertEqual "Expected parser to ignore whitespace string" (parseJSON "   \"abc\"   ") (Right (JSONString "abc")),
        assertEqual "Expected parser to ignore whitespace boolean" (parseJSON "   true   ") (Right (JSONBool True)),
        assertEqual "Expected null to yield null" (parseJSON "   null   ") (Right JSONNull),
        assertEqual "Expected array to be parsed" (parseJSON " [\nnull, \"qwe\",\t true ] ") (Right (JSONArray [JSONNull, JSONString "qwe", JSONBool True]))
    ]
