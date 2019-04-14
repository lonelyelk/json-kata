module JSON (JSON(..), parseJSON) where

import Text.Parsec
import Text.Parsec.String
import Control.Monad (void)

data JSON = JSONString String
          | JSONBool Bool
          | JSONNull
          | JSONArray [JSON]
          | JSONObject [(String, JSON)]
          deriving (Show, Eq)

parseJSON :: String -> Either ParseError JSON
parseJSON = parse (whiteSpace *> jsonValue) "json"

jsonValue :: Parser JSON
jsonValue = choice [jsonString, jsonBool, jsonNull, jsonArray, jsonObject]

jsonString :: Parser JSON
jsonString = JSONString <$> (between (char '"') (char '"') (many (noneOf "\"\\")) <* whiteSpace)

jsonBool :: Parser JSON
jsonBool = (JSONBool True <$ atom "true") <|> (JSONBool False <$ atom "false")

jsonNull :: Parser JSON
jsonNull = JSONNull <$ atom "null"

jsonArray :: Parser JSON
jsonArray = JSONArray <$> whiteSpacedBetween '[' ']' (jsonValue `sepBy` whiteSpacedChar ',')

jsonObject :: Parser JSON
jsonObject = JSONObject <$> whiteSpacedBetween '{' '}' (keyValPair `sepBy` whiteSpacedChar ',')
    where
        keyValPair = do
            JSONString key <- jsonString
            whiteSpacedChar ':'
            value <- jsonValue
            return (key, value)

whiteSpace :: Parser ()
whiteSpace = void $ many $ oneOf " \n\t\r"

atom :: String -> Parser String
atom str = string str <* whiteSpace

whiteSpacedChar :: Char -> Parser ()
whiteSpacedChar ch = char ch *> whiteSpace

whiteSpacedBetween :: Char -> Char -> Parser a -> Parser a
whiteSpacedBetween start finish values =
    between (whiteSpacedChar start) (whiteSpacedChar finish) values <* whiteSpace


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
        assertEqual "Expected array to be parsed" (parseJSON " [\nnull, \"qwe\",\t true ] ") (Right (JSONArray [JSONNull, JSONString "qwe", JSONBool True])),
        assertEqual "Expected empty array to be parsed" (parseJSON " [\n] ") (Right (JSONArray [])),
        assertEqual "Expected object to be parsed" (parseJSON " {\n \"key\": [\n\"value\" \n] } ") (Right (JSONObject [("key", JSONArray [JSONString "value"])]))
    ]
