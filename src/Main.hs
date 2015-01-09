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
  
  import Data.Char            (isSpace)

  import System.IO          (hFlush, stdout)
  import System.Environment (getArgs)

  import Control.Monad.Trans.Either (eitherT)
  
  import Query  (fetch_query)
  import Parser (parse_fsql)
  
  
  
  usage = putStrLn $ "Enter query statement or quit|exit to leave.\n\
                    \FSQL can also be used straight from the command line: \
                    \fsql <query>\n
                    \Please note that where statements should be quoted in the \
                    \commandline, as they may be interpreted as shell commands."

  main = getArgs >>= \case []   -> usage >> command_loop
                           args -> either print_err (print_query . fetch_query)
                                   $ parse_fsql (unwords args)
    where
      flushStr s = putStr s >> hFlush stdout
      putStrLn'  = putStrLn . (++ "\n")

      print_query = eitherT putStrLn' (putStrLn . unlines)
      print_err   = putStrLn' . show
      
      lexeme l (lex -> [(l', s)]) =  l == l'  &&  all isSpace s
      lexeme _ _ = False
      
      command_loop =
        do flushStr "fsql> "
           s <- getLine
           if lexeme "exit" s  ||  lexeme "quit" s
            then return ()
            else either print_err (print_query . fetch_query) (parse_fsql s)
                  >> command_loop
