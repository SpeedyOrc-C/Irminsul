module Main where

import Web.Scotty

import System.Directory

import Root (root)
import Showcase
import LanguagePack
import System.Environment (getArgs)
import Data.List (isSuffixOf)

waitOk :: String -> IO () -> IO ()
waitOk info action = do
    putStr info
    action
    putStrLn "OK"

getOutputDirectory :: [String] -> String
getOutputDirectory [] = "./irminsul-output"
getOutputDirectory (d:_) =
    if "/" `isSuffixOf` d then take (length d - 1) d else d

debugGenerate :: IO ()
debugGenerate = do
    let showcases = flattenShowcases root
    createShowcaseDirectory "./irminsul-output" `mapM_` showcases
    sequence_ $ do
        showcase <- showcases
        language <- languages
        let outputPath = showcaseOutputPath "./irminsul-output" language showcase
            outputHtml = showcaseHtml language showcase
        return $ do
            putStrLn $ "[WRITING] " ++ outputPath
            writeFile outputPath (show outputHtml)

main :: IO ()
main = do
    args <- getArgs

    let outputDirectory = getOutputDirectory args
        showcases = flattenShowcases root

    putStrLn $ "Output directory: " ++ outputDirectory

    waitOk "Creating directories for output..."
        (createShowcaseDirectory outputDirectory `mapM_` showcases)

    putStrLn $ show (length showcases) ++ " showcase(s)"
    putStrLn $ show (length languages) ++ " language(s)"
    putStrLn $ show (length languages * length showcases) ++ " file(s) in total"

    sequence_ $ do
        showcase <- showcases
        language <- languages
        let outputPath = showcaseOutputPath outputDirectory language showcase
            outputHtml = showcaseHtml language showcase
        return $ do
            putStrLn $ "[WRITING] " ++ outputPath
            writeFile outputPath (show outputHtml)
