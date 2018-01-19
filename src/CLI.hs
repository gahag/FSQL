{- FSQL : CLI.hs -- Command line interface - interactive command line
 -
 - Copyright (C) 2015 gahag
 - All rights reserved.
 -
 - This software may be modified and distributed under the terms
 - of the BSD license. See the LICENSE file for details.
 -}

module CLI (
    fsql_cli
  ) where
  
  import Control.Monad  (unless)
  import Data.Char      (isSpace)
  
  import System.IO  (hFlush, stdout)
  
  import FSQL (fsql_run)
  
  
  fsql_cli :: IO ()
  fsql_cli = do putStr "fsql> " >> hFlush stdout
                s <- getLine
                unless (lexeme "exit" s || lexeme "quit" s)
                 $ fsql_run "interactive" s >> fsql_cli
    where
      -- lexeme : checks if a string equals to the especified lexeme.
      lexeme :: String -> String -> Bool
      lexeme l s | [(l', s')] <- lex s =  l == l' && all isSpace s'
                 | otherwise = False
