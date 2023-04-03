module Data.Xml (Xml(..), html, div, img) where

import Prelude hiding (div, head, id, span)
import LanguagePack

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

html lang titleContent cssLinks bodyContent =
    Tag "html" [("lang", case lang of {
        ZhCn -> "zh-cn";
        EnUs -> "en-us";
    })] [
        head ([metaUtf8] ++ map linkCss cssLinks ++ [title titleContent]),
        body bodyContent
    ]

head = Tag "head" []
body = Tag "body" []
div = Tag "div"
span = Tag "span"
img src alt = Tag "img" [("src", src), ("alt", alt)]
title t = Tag "title" [] [Text t]
metaUtf8 = TagClosing "meta" [("charset", "UTF-8")]
linkCss href = TagClosing "link" [("rel", "stylesheet"), ("href", href)]

id_ = (,) "id"
class_ = (,) "class"
style_ = (,) "style"

(.=) = (,)
