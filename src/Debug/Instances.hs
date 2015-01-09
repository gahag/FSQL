{- FSQL : Debug/Instances.hs -- Type instances for debuging
 -
 - Copyright (C) 2015 gahag
 - All rights reserved.
 -
 - This software may be modified and distributed under the terms
 - of the BSD license.  See the LICENSE file for details.
 -}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}

module Debug.Instances where
  
  import Prelude hiding (Either(..))
  import qualified Prelude as P (Either(..))
  
  import Query  (Query(..), Selection(..), Source(..), Join(..), Predicate)
  import Expr   (Expr(..), Value(..))
  
  
  
  instance Show Expr where
    show (Not e)  = " !" ++ show e ++ " "
    show (BoolOp o e e') = concat ["(", show e, " ", show o, " ", show e', ")"]
    show (RelOp  o s v)  = concat ["(", show s, " ", show o, " ", show v, ")"]
  
  instance Show Value where
    show = \case RawVal  s -> s
                 DayVal  d -> show d
                 SizeVal s -> show s
  
  instance Show (P.Either Value Selection) where
    show = either show show
  
  instance Show Query where
    show (Query s s' p) = concat ["Query ", show s, " ", show s' , " ", show p]
  
  instance Show Source where
    show (Single s)     = show s
    show (Join j s sel) = concat [show j, " Join ", show s, " On ", show sel]
  
  instance Show Join where
    show = \case Inner -> "Inner"
                 Outer -> "Outer"
                 Left  -> "Left"
                 Right -> "Right"
                 Full  -> "Full"
  
  instance Show Predicate where
    show = const "Predicate"
  
  instance Show (Maybe Predicate) where
    show = maybe "Nothing" show
