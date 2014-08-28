{-
    Dolce - Main.hs
    Copyright (c) 2014, Susisu
    see: license.txt
-}

module Main (
    main
) where

import Control.Exception
import qualified System.Environment as Env
import qualified System.FilePath as FilePath
import qualified System.IO.UTF8 as UTF8

import Parser
import VM
import VM.Data
import VM.Platform.DOS


main :: IO ()
main = do
        args <- Env.getArgs
        case args of
            "--version" : []   -> showVersionInfo
            ipath : opath : [] -> compile ipath opath
            ipath : []         -> compile_ ipath
            _                  -> showHelp

compile :: FilePath -> FilePath -> IO ()
compile ipath opath = handle handler $ do
        source <- UTF8.readFile ipath
        case parse (FilePath.takeFileName ipath) source of
            Left err -> putStrLn $ "Error: " ++ show err
            Right op -> do
                    platform <- newDOSPlatform
                    state    <- newVMState platform
                    res      <- runOperation op state
                    case res of
                        Left err -> putStrLn $ show err
                        Right _  -> output platform >>= UTF8.writeFile opath
    where
        handler :: SomeException -> IO ()
        handler exception = putStrLn $ "Error: " ++ show exception

compile_ :: FilePath -> IO ()
compile_ ipath = compile ipath opath
    where
        opath = if FilePath.takeExtension ipath == ".dol" then
                FilePath.replaceExtension ipath "dos"
            else
                FilePath.addExtension ipath "dos"

showVersionInfo :: IO ()
showVersionInfo = do
        putStrLn "Dolce - the DOL CompilEr"
        putStrLn "Version 0.2.1"
        putStrLn "Copyright(C) 2014 Susisu http://susisu.ktkr.net/"

showHelp :: IO ()
showHelp = do
        showVersionInfo
        putStrLn ""
        putStrLn "Usage:"
        putStrLn "<infile> [outfile] : compile infile to outfile"
        putStrLn "--version : display version info"

