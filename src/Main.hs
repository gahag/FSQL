{- FSQL : Main.hs -- Program entry point - command line handling
 -
 - Copyright (C) 2015 gahag
 - All rights reserved.
 -
 - This software may be modified and distributed under the terms
 - of the BSD license. See the LICENSE file for details.
 -}

{-# LANGUAGE LambdaCase, ViewPatterns #-}

module Main where
  
  import Control.Monad.Trans.Except (runExceptT)
  import Data.Char                  (isSpace)

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

  main = getArgs >>= \case []   -> putStr usage >> command_loop
                           args -> parse_fetch_query (unwords args)
    where
      putStrLn'  = putStrLn  . (++ "\n")
      print_err  = putStrLn' . show
      
      lexeme l s | [(l', s')] <- lex s =  l == l'  &&  all isSpace s'
                 | otherwise =  False
      
      parse_fetch_query = either print_err do_query . parse_fsql
      do_query q = runExceptT (fetch_query q)
               >>= either putStrLn' (putStrLn . unlines)
      
      
      command_loop =
        do putStr "fsql> " >> hFlush stdout
           s <- getLine
           if lexeme "exit" s || lexeme "quit" s
            then return ()
            else parse_fetch_query s >> command_loop
