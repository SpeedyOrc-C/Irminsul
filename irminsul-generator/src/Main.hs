module Main where

import System.Directory (createDirectoryIfMissing)

import Root (root)
import Showcase
import LanguagePack

main :: IO ()
main = do
    let showcases = flattenShowcases root
    createShowcaseDirectory `mapM_` showcases
    sequence_ $ do
        showcase <- showcases
        language <- languages
        let outputPath = showcaseOutputPath language showcase
        let outputHtml = showcaseHtml language showcase
        return $ do
            writeFile outputPath (show outputHtml)
