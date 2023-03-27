module Main where

import System.IO
import System.Environment
import Data.List

import Root
import Showcase
import LanguagePack


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
    stdin `hSetEncoding` utf8
    stdout `hSetEncoding` utf8

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
            withFile outputPath WriteMode $ \f -> do
                -- Write files in UTF-8
                -- Because Windows always use the local encoding.
                f `hSetEncoding` utf8
                f `hPutStr` show outputHtml
