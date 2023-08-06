module Translation.ZhCn.Utility where

{- |
    Intercalate every parts of name with a Chinese separator "·",
    avoiding misuse of punctuation.

    >>> "马库斯"."阿列克谢"."泊松"
    "马库斯·阿列克谢·泊松"
-}
(.) :: String -> String -> String
x . y = x ++ "·" ++ y
