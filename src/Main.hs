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
  
  import Data.Version (showVersion)
  
  import System.Environment (getArgs)
  
  import CLI  (fsql_cli)
  import FSQL (fsql_run)
  import Paths_FSQL (version)
  
  usage :: String
  usage = unlines
    [ "FSQL v" ++ showVersion version ++ " - hosted at \
      \https://github.com/gahag/FSQL"
    , "Enter query statement or quit|exit to leave."
    , "FSQL can also be used from the command line: fsql <query>"
    , "Please note that when using FSQL from the command line, where statements\
      \ should be quoted because they may be interpreted as shell commands. \
      \Also, Double quotes must be escaped." ]
  
  main :: IO ()
  main = getArgs >>= \case []   -> putStrLn usage >> fsql_cli
                           args -> fsql_run "command line" (unwords args)
