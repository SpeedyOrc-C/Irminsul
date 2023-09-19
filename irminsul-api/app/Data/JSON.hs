module Data.JSON where

import Data.List (intercalate)
import Data.Char (ord)
import Numeric (showHex)

data JSON
    = JNull
    | JBool Bool
    | JNumber Double
    | JString String
    | JArray [JSON]
    | JObject [(String, JSON)]

class ToJSON a where
    toJSON :: a -> JSON

escapeChar :: Char -> String
escapeChar char
    | char == '"'  = "\\\""
    | char == '\\' = "\\\\"
    | char == '\b' = "\\\b"
    | char == '\f' = "\\\f"
    | char == '\n' = "\\\n"
    | char == '\r' = "\\\r"
    | otherwise = return char

escapeCharUnicode :: Char -> String
escapeCharUnicode char
    | 0 <= code && code <= 255 = return char
    | otherwise = "\\u" ++ replicate (4 - length hexCode) '0' ++ hexCode
    where
        code = ord char
        hexCode = showHex code ""

instance Show JSON where
    show :: JSON -> String
    show JNull              = "null"
    show (JBool True)       = "true"
    show (JBool False)      = "false"
    show (JNumber number)   = show number
    show (JString string)   = "\"" ++ inner ++ "\"" where
        inner = string >>= escapeCharUnicode
    show (JArray array)     = "[" ++ inner ++ "]" where
        inner = intercalate "," (map show array)
    show (JObject object)   = "{" ++ inner ++ "}" where
        inner = intercalate "," pairs where
            pairs = map (\(key, value) -> show key ++ ":" ++ show value) object

instance Semigroup JSON where
    (<>) :: JSON -> JSON -> JSON
    (JArray xs) <> (JArray ys) = JArray $ xs <> ys
    (JArray xs) <> y = JArray $ xs ++ [y]
    x <> (JArray ys) = JArray $ x : ys
    x <> y = JArray [x, y]
