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
  
  import Control.Monad        (unless, when)
  import Control.Monad.Trans  (lift)
  
  import System.IO                (hIsTerminalDevice, stdin, stdout)
  import System.Console.Haskeline (InputT, defaultSettings
                                  , getInputLine, runInputT, outputStrLn)
  
  import FSQL     (fsql_run)
  import Version  (aboutMsg)
  
  
  usage :: String
  usage = unlines
    [ aboutMsg
    , "Enter query statement or quit|exit to leave."
    , "FSQL can also be used from the command line: fsql <query>"
    , "Please note that when using FSQL from the command line, where statements\
      \ should be quoted because they may be interpreted as shell commands. \
      \Also, Double quotes must be escaped." ]
  
  
  fsql_cli :: IO ()
  -- runInputT: Uses terminal-style interaction if stdin is connected to a terminal and
  -- has echoing enabled. Otherwise (e.g., if stdin is a pipe), it uses file-style interaction.
  fsql_cli = runInputT defaultSettings
             $ do stdinTTY  <- lift $ hIsTerminalDevice stdin
                  stdoutTTY <- lift $ hIsTerminalDevice stdout
                  -- If neither stdin or stdout are in the terminal, ommit usage:
                  when (stdinTTY || stdoutTTY) (outputStrLn usage)
                  loop stdinTTY
    where
      loop :: Bool -> InputT IO ()
      loop stdinTTY = getInputLine "fsql> "
                  >>= \case Nothing -> do -- Caught EOF.
                                        -- When in a tty, Ctrl-D (EOF) breaks the line.
                                        -- Otherwise, output the line break manually:
                                        unless stdinTTY $ outputStrLn ""
                                        outputStrLn "Leaving fsql."
                            
                            Just "exit" -> return ()
                            Just "quit" -> return ()
                            
                            Just input -> do -- Query input.
                                            -- Echo the input if not already echoed:
                                            unless stdinTTY $ outputStrLn input
                                            _ <- lift $ fsql_run "interactive" input
                                            loop stdinTTY
