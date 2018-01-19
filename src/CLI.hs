{- FSQL : CLI.hs -- Command line interface - interactive command line
 -
 - Copyright (C) 2015 gahag
 - All rights reserved.
 -
 - This software may be modified and distributed under the terms
 - of the BSD license. See the LICENSE file for details.
 -}

{-# LANGUAGE LambdaCase #-}

module CLI (
    fsql_cli
  ) where
  
  import Control.Monad.Trans  (lift)
  
  import System.Console.Haskeline (InputT, defaultSettings, getInputLine, runInputT)
  
  import FSQL (fsql_run)
  
  
  fsql_cli :: IO ()
  fsql_cli = runInputT defaultSettings loop
   where
     loop :: InputT IO ()
     loop = getInputLine "fsql> "
        >>= \case Nothing     -> lift (putStrLn "Leaving fsql.") -- EOF
                  Just "exit" -> return ()
                  Just "quit" -> return ()
                  Just input  -> lift (fsql_run "interactive" input) >> loop
