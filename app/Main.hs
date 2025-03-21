{-# LANGUAGE GADTs #-}

module Main where

import Control.Applicative (Alternative (empty, many, (<|>)))
import Control.Monad (replicateM, (>=>))
import Data.Char (chr, isDigit, isHexDigit, isSpace)
import Data.Tuple (swap)
import Numeric (readHex)
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)

data JsonValue
    = JsonObject [(String, JsonValue)]
    | JsonArray [JsonValue]
    | JsonString String
    | JsonNumber Double
    | JsonBool Bool
    | JsonNull
    deriving (Read, Show)

newtype Parser a where
    Parser :: {runP :: String -> Maybe (String, a)} -> Parser a

instance Functor Parser where
    fmap f p = Parser $ runP p >=> (\(ys, a) -> Just (ys, f a))

instance Applicative Parser where
    pure x = Parser $ \input -> Just (input, x)
    Parser p1 <*> Parser p2 =
        Parser $ \input -> do
            (input', f) <- p1 input
            (input'', a) <- p2 input'
            Just (input'', f a)

instance Alternative Parser where
    empty = Parser $ const Nothing
    Parser p1 <|> Parser p2 = Parser $ \input -> p1 input <|> p2 input

jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ stringP "null"

jsonBool :: Parser JsonValue
jsonBool = JsonBool . readBool <$> (stringP "true" <|> stringP "false")
  where
    readBool "true" = True
    readBool "false" = False
    readBool _ = undefined

-- | Build a Double from its parts (sign, integral part, decimal part, exponent)
doubleFromParts :: Integer -> Integer -> Double -> Integer -> Double
doubleFromParts sign int dec expo =
    fromIntegral sign * (fromIntegral int + dec) * (10 ^^ expo)

jsonNumber :: Parser JsonValue
jsonNumber =
    (JsonNumber <$>) $
        doubleFromParts
            <$> (minus <|> pure 1)
            <*> (read <$> digits)
            <*> ((read . ('0' :) <$> ((:) <$> charP '.' <*> digits)) <|> pure 0)
            <*> ((e *> ((*) <$> (plus <|> minus <|> pure 1) <*> (read <$> digits))) <|> pure 0)
  where
    minus = (-1) <$ charP '-' :: Parser Integer
    plus = 1 <$ charP '+' :: Parser Integer
    digits = isNull $ spanP isDigit :: Parser String
    e = charP 'e' <|> charP 'E'

jsonString :: Parser JsonValue
jsonString = (JsonString <$>) $ quote *> many (normalChar <|> escapeChar) <* quote
  where
    quote = charP '"'
    normalChar = charIfP $ (&&) <$> (/= '"') <*> (/= '\\')
    escapeUnicode = chr . fst . head . readHex <$> replicateM 4 (charIfP isHexDigit)
    escapeChar =
        ('"' <$ stringP "\\\"")
            <|> ('\\' <$ stringP "\\\\")
            <|> ('/' <$ stringP "\\/")
            <|> ('\b' <$ stringP "\\b")
            <|> ('\f' <$ stringP "\\f")
            <|> ('\n' <$ stringP "\\n")
            <|> ('\r' <$ stringP "\\r")
            <|> ('\t' <$ stringP "\\t")
            <|> (stringP "\\u" *> escapeUnicode)

jsonArray :: Parser JsonValue
jsonArray = (JsonArray <$>) $ charP '[' *> separateByP (charP ',') jsonValue <* charP ']'

jsonObject :: Parser JsonValue
jsonObject =
    (JsonObject <$>) $ charP '{' *> wsP *> separateByP (charP ',') keyVal <* wsP <* charP '}'
  where
    extractString (JsonString xs) = xs
    extractString _ = undefined -- "tried to extract string from non json string"
    keyVal = wsP *> ((,) . extractString <$> jsonString <* wsP <* charP ':' <* wsP) <*> jsonValue

jsonValue :: Parser JsonValue
jsonValue = wsP *> (jsonString <|> jsonNumber <|> jsonObject <|> jsonArray <|> jsonBool <|> jsonNull) <* wsP

-- | parses a Char
charP :: Char -> Parser Char
charP c = charIfP (== c)

-- | parses a String
stringP :: String -> Parser String
stringP = traverse charP

-- | Parses a char satisfying a predicate
charIfP :: (Char -> Bool) -> Parser Char
charIfP p = Parser f
  where
    f (x : xs)
        | p x = Just (xs, x)
        | otherwise = Nothing
    f [] = Nothing

-- | Parses a string of Chars satisfying a predicate
spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ Just . swap . span f

wsP :: Parser String
wsP = spanP isSpace

-- | checks if a list is null and returns Nothing if so
isNull :: (Foldable t) => Parser (t a) -> Parser (t a)
isNull (Parser p) = Parser $ \input -> do
    (rest, token) <- p input
    if null token then Nothing else return (rest, token)

{- | Creates a parser for a string of type "element1 sep1 element2 sep2 element3"
from a parser for separators (sep1, sep2) and and a parser form elements (element1, element2, element3).
-}
separateByP :: Parser a -> Parser b -> Parser [b]
separateByP sep element = (:) <$> element <*> many (sep *> element) <|> pure []

main :: IO ()
main = do
    fileNames <- getArgs >>= parseArgs
    fileContents <- mapM readFile fileNames
    let parsedFiles = zip fileNames (map (runP jsonValue) fileContents)
    print parsedFiles

-- | gets file names to parse
parseArgs :: [String] -> IO [String]
parseArgs [] = exitFailure
parseArgs ["-h"] = usage >> exitSuccess
parseArgs ["-v"] = version >> exitSuccess
parseArgs files = return files

usage :: IO ()
usage = putStrLn "Usage: [-hv] [file ..]"

version :: IO()
version = putStrLn "json-parser-hs version 1.0"
