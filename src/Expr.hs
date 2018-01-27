{- FSQL : Expr.hs -- Expression functions
 -
 - Copyright (C) 2015 gahag
 - All rights reserved.
 -
 - This software may be modified and distributed under the terms
 - of the BSD license. See the LICENSE file for details.
 -}
  
module Expr (
    Expr, TypeError(..),
    compile
  ) where
  
  import Control.Monad.Except (ExceptT(..))
  
  import           Expr.Untyped          (Expr)
  import qualified Expr.Typed       as T (compile)
  import           Expr.TypeChecker      (TypeError(..), typecheck)
  import           Query                 (Predicate)
  
  
  compile :: (Monad m) => Expr -> ExceptT TypeError m Predicate
  compile e = T.compile <$> typecheck e
