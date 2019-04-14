module JSON (JSON(..), parseJSON) where

import Text.Parsec
import Text.Parsec.String
import Control.Monad (void)
import Data.Char

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
jsonString = JSONString <$> (between (char '"') (char '"') (many jsonStringChar) <* whiteSpace)
    where
        jsonStringChar = noneOf "\"\\" <|> (char '\\' *> escapedChar)
        escapedChar = ('\n' <$ char 'n')
            <|> ('\t' <$ char 't')
            <|> ('\r' <$ char 'r')
            <|> ('\b' <$ char 'b')
            <|> ('\f' <$ char 'f')
            <|> ('\\' <$ char '\\')
            <|> ('"' <$ char '"')
            <|> utf8Char

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

utf8Char :: Parser Char
utf8Char = do
    char 'u'
    hex <- count 4 hexDigit
    return (chr (foldl (\n c -> n*16 + hexDigitVal (toLower c)) 0 hex))

hexDigitVal :: Char -> Int
hexDigitVal d
    | d == '0' = 0
    | d == '1' = 1
    | d == '2' = 2
    | d == '3' = 3
    | d == '4' = 4
    | d == '5' = 5
    | d == '6' = 6
    | d == '7' = 7
    | d == '8' = 8
    | d == '9' = 9
    | d == 'a' = 10
    | d == 'b' = 11
    | d == 'c' = 12
    | d == 'd' = 13
    | d == 'e' = 14
    | d == 'f' = 15
    | otherwise = 0

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
        assertEqual "Expected object to be parsed" (parseJSON " {\n \"key\": [\n\"value\" \n] } ") (Right (JSONObject [("key", JSONArray [JSONString "value"])])),
        assertEqual "Expected escaped chars in string to be parsed" (parseJSON " \"\\n\\t \\\\ \\\" \\b\" ") (Right (JSONString "\n\t \\ \" \b")),
        assertEqual "Expected utf-8 encoded chars in string to be parsed" (parseJSON " \"\\u2764 \\u00FC\" ") (Right (JSONString "❤ ü"))
    ]
