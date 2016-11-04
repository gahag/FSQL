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
  
  import Control.Monad        (unless)
  import Control.Monad.Except (runExceptT)
  import Data.Char            (isSpace)
  import Data.Function        (on)
  import Data.List            (intercalate, maximumBy, transpose)
  import Data.Version         (showVersion)
  
  import System.IO          (hFlush, stdout)
  import System.Environment (getArgs)
  
  import Query      (fetch_query)
  import Parser     (parse_fsql)
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
                           args -> fetch_fsql . parse_fsql "command line"
                                                          $ unwords args
    where
      putStrLn' s = putStrLn . (++ "\n")
      
      print_cols = putStr . unlines . map (intercalate "\t")
      
      
      -- lexeme : checks if a string equals to the especified lexeme.
      lexeme l s | [(l', s')] <- lex s =  l == l'  &&  all isSpace s'
                 | otherwise = False
      
      fetch_fsql = either (putStrLn' . show) do_query
      
      do_query q = runExceptT (fetch_query q)
               >>= either putStrLn' print_cols
      
      
      command_loop =
        do putStr "fsql> " >> hFlush stdout
           s <- getLine
           unless (lexeme "exit" s || lexeme "quit" s)
            $ fetch_fsql (parse_fsql "interactive" s)
              >> command_loop
