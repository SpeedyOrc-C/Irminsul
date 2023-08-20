module Data.Xml (Xml(..)) where

data Xml
    = Text String
    | TagClosing String [(String, String)]
    | Tag String [(String, String)] [Xml]

instance Show Xml where
    show (Text s) = s
    show (TagClosing tag pairs) =
        "<" ++ tag
        ++ concatMap (\(key, attribute) ->
            " " ++ key ++ "=\"" ++ attribute ++ "\"") pairs
        ++ ">"
    show (Tag tag pairs xmls) =
        show (TagClosing tag pairs)
        ++ concatMap show xmls
        ++ "</" ++ tag ++ ">"
