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
  
  import System.Environment (getArgs)
  
  import CLI      (fsql_cli)
  import FSQL     (fsql_run)
  import Version  (aboutMsg)
  
  
  main :: IO ()
  main = getArgs >>= \case []              -> fsql_cli
                           [v] | version v -> putStrLn aboutMsg
                           args            -> fsql_run "command line" (unwords args)
    where
      version v = v `elem` ["-v", "--version"]
