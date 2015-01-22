{- FSQL : Main.hs -- Program entry point - command line handling
 -
 - Copyright (C) 2015 gahag
 - All rights reserved.
 -
 - This software may be modified and distributed under the terms
 - of the BSD license. See the LICENSE file for details.
 -}

{-# LANGUAGE LambdaCase #-}

module Main where
  
  import Control.Arrow        (left)
  import Control.Monad.Except (ExceptT(..), runExceptT, withExceptT)
  import Data.Char            (isSpace)

  import System.IO          (hFlush, stdout)
  import System.Environment (getArgs)
  
  import Query  (fetch_query)
  import Parser (parse_fsql)
  
  
  
  usage = unlines
    [ "Enter query statement or quit|exit to leave."
    , "FSQL can also be used straight from the commandline: fsql <query>"
    , "Please note that where statements should be quoted in the \
      \commandline, as they may be interpreted as shell commands. \
      \Double quotes also must be escaped with backslash." ]
  
  main = getArgs >>= \case []   -> putStrLn usage >> command_loop
                           args -> fetch_fsql . parse_fsql "command line"
                                                          $ unwords args
    where
      putStrLn' = putStrLn . (++ "\n")
      
      -- lexeme : checks if a string equals to the especified lexeme.
      lexeme l s | [(l', s')] <- lex s =  l == l'  &&  all isSpace s'
                 | otherwise = False
      
      fetch_fsql = either (putStrLn' . show) do_query
      
      do_query q = runExceptT (fetch_query q)
               >>= either putStrLn' (putStrLn . unlines)
      
      
      command_loop =
        do putStr "fsql> " >> hFlush stdout
           s <- getLine
           if lexeme "exit" s || lexeme "quit" s
            then return ()
            else fetch_fsql (parse_fsql "interactive" s)
              >> command_loop
