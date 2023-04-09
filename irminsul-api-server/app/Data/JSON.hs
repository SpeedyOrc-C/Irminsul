module Data.JSON where

import Data.List (intercalate)
import Data.Char
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
    | char == '"' = "\\\""
    | char == '\\' = "\\\\"
    | char == '\b' = "\\\b"
    | char == '\f' = "\\\f"
    | char == '\n' = "\\\n"
    | char == '\r' = "\\\r"
    | otherwise = return char
    -- | 0 <= code && code <= 255 = return char
    -- | otherwise = "\\u" ++ replicate (4 - length hexCode) '0' ++ hexCode
    where
        code = ord char
        hexCode = showHex code ""

instance Show JSON where
    show JNull = "null"
    show (JBool bool) = if bool then "true" else "false"
    show (JNumber number) = show number
    show (JString string) = "\"" ++ finalInner ++ "\"" where
        finalInner = do
            char <- string
            escapeChar char
    show (JArray array) = "[" ++ intercalate "," (map show array) ++ "]"
    show (JObject object) = "{" ++ intercalate ","
        (map (\(key, value) -> show key ++ ":" ++ show value) object) ++ "}"
