{- FSQL : Main.hs -- Program entry point - command line handling
 -
 - Copyright (C) 2015 gahag
 - All rights reserved.
 -
 - This software may be modified and distributed under the terms
 - of the BSD license. See the LICENSE file for details.
 -}

{-# LANGUAGE LambdaCase #-}

module Main (
    main
  ) where
  
  import Data.Functor (($>))
  
  import System.Environment (getArgs)
  import System.Exit        (ExitCode(..), exitWith)
  import System.IO          (hPutStr, stderr)
  
  import CLI      (fsql_cli)
  import Version  (aboutMsg)
  
  
  main :: IO ()
  main = getArgs >>= fsql_main >>= exitWith
  
  fsql_main :: [String] -> IO ExitCode
  fsql_main = \case []              -> fsql_cli $> ExitSuccess
                    [v] | version v -> putStrLn aboutMsg $> ExitSuccess
                    args            -> hPutStr stderr (
                                        unlines [ "Error:"
                                                , "Invalid arguments: " ++ show args ]
                                       ) $> ExitFailure 1
    where
      version v = v `elem` ["-v", "--version"]
