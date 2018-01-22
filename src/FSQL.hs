{- FSQL : FSQL.hs -- Main FSQL functions.
 -
 - Copyright (C) 2015 gahag
 - All rights reserved.
 -
 - This software may be modified and distributed under the terms
 - of the BSD license. See the LICENSE file for details.
 -}

{-# LANGUAGE LambdaCase #-}

module FSQL (
    fsql_run
  ) where
  
  import Data.Functor         (($>))
  import Control.Arrow        (left)
  import Control.Monad.Except (ExceptT(..), runExceptT, withExceptT)
  import Data.List            (intercalate)
  
  import System.Exit      (ExitCode(..))
  import System.IO        (hPutStrLn, stderr)
  import System.IO.Error  (IOError, tryIOError)
  
  import Query  (FetchError, fetch_query)
  import Parser (ParseError, fsql_parse)
  
  
  data Error = ParseErr ParseError
             | FetchErr FetchError
             | IOErr    IOError
  
  
  fsql_run :: String -> String -> IO ExitCode
  fsql_run name input =
    do query  <- withExceptT ParseErr $ fsql_parse name input
       result <- withExceptT FetchErr $ fetch_query query
       print_result (unlines $ map (intercalate "\t") result) $> ExitSuccess
    `handleError` (
      \case ParseErr e -> putErr ("Parse error:\n" ++ show e)  $> ExitFailure 1
            FetchErr e -> putErr ("IO error:\n" ++ show e)     $> ExitFailure 2
            IOErr    e -> putErr ("Output error:\n" ++ show e) $> ExitFailure 3
    )
    where
      print_result :: String -> ExceptT Error IO ()
      print_result = ExceptT . fmap (left IOErr) . tryIOError . putStrLn
      
      handleError :: (Monad m) => ExceptT e m a -> (e -> m a) -> m a
      handleError e f = runExceptT e >>= either f return
      
      putErr :: String -> IO ()
      putErr = hPutStrLn stderr
