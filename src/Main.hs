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
  
  import Control.Arrow        (left)
  import Control.Monad        (unless)
  import Control.Monad.Except (ExceptT(..), runExceptT)
  import Data.Char            (isSpace)
  import Data.Function        (on)
  import Data.List            (intercalate, maximumBy, transpose)
  import Data.Version         (showVersion)
  
  import System.IO          (hFlush, stdout)
  import System.Environment (getArgs)
  
  import Query                 (Query, fetch_query)
  import qualified Parser as P (parse_fsql)
  
  import Paths_FSQL (version)
  
  
  
  usage = unlines
    [ "FSQL v" ++ showVersion version ++ " - hosted at \
      \https://github.com/gahag/FSQL"
    , "Enter query statement or quit|exit to leave."
    , "FSQL can also be used from the command line: fsql <query>"
    , "Please note that when using FSQL from the command line, where statements\
      \ should be quoted because they may be interpreted as shell commands. \
      \Also, Double quotes must be escaped." ]
  
  main = getArgs >>= \case []   -> putStrLn usage >> command_loop
                           args -> print_fsql
                                 $ fetch_fsql
                               =<< parse_fsql "command line" (unwords args)
    where
      -- lexeme : checks if a string equals to the especified lexeme.
      lexeme :: String -> String -> Bool
      lexeme l s | [(l', s')] <- lex s =  l == l' && all isSpace s'
                 | otherwise = False
      
      
      parse_fsql :: String -> String -> ExceptT String IO Query
      parse_fsql name source = ExceptT . return . left show
                             $ P.parse_fsql name source
      
      fetch_fsql :: Query -> ExceptT String IO String
      fetch_fsql q = unlines . map (intercalate "\t")
                  <$> fetch_query q
      
      print_fsql :: ExceptT String IO String -> IO ()
      print_fsql result = runExceptT result
                      >>= either putStrLn putStr
      
      
      command_loop =
        do putStr "fsql> " >> hFlush stdout
           s <- getLine
           unless (lexeme "exit" s || lexeme "quit" s)
            $ print_fsql (parse_fsql "interactive" s >>= fetch_fsql)
              >> command_loop
