{- FSQL : Regex.hs -- Regex TDFA functions
 -
 - Copyright (C) 2015 gahag
 - All rights reserved.
 -
 - This software may be modified and distributed under the terms
 - of the BSD license. See the LICENSE file for details.
 -}

module Regex (
    Regex, makeRegex, match
  ) where
  
  import Control.Monad.Except (ExceptT(..), throwError)
  
  import Text.Regex.TDFA  (Regex, makeRegexM, match)
  
  
  -- makeRegex ---------------------------------------------------------------------------
  data FailMonad a = Failure String -- Monad to capture the string from `fail`.
                   | Success a
  
  instance Functor FailMonad where
    fmap _ (Failure s) = Failure s
    fmap f (Success x) = Success (f x)
  
  instance Applicative FailMonad where
    pure            = Success
    Failure s <*> _ = Failure s
    Success f <*> r = fmap f r

  instance Monad FailMonad where
    Failure s >>= _ = Failure s
    Success r >>= f = f r
    
    fail = Failure  -- Capture the string.
  
  
  makeRegex :: (Monad m) => String -> ExceptT String m Regex
  makeRegex s = case makeRegexM s of Failure s -> throwError s
                                     Success r -> return r
  -- -------------------------------------------------------------------------------------
