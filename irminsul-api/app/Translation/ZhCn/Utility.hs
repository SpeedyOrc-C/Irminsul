module Translation.ZhCn.Utility where
import Data.String (IsString)

{- |
    Intercalate every parts of name with a Chinese separator "·",
    avoiding misuse of punctuation.

    >>> "马库斯"."阿列克谢"."泊松"
    "马库斯·阿列克谢·泊松"
-}
(.) :: (Semigroup a, IsString a) => a -> a -> a
x . y = x <> "·" <> y
