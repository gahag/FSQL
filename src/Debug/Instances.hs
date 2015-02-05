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
  
  import Query  (Query(..), Selection(..), Source(..), Join(..), Predicate)
  import Expr   (Expr(..), Op(..))
  
  
  
  instance Show Expr where
    show (Atom a) = show a
    show (Not e)  = " !" ++ show e ++ " "
    show (Op op e e') = concat ["(", show e, " ", show op, " ", show e', ")"]
  
  instance Show Op where
    show = \case And     -> "&&"
                 Or      -> "||"
                 Less    -> "<"
                 Greater -> ">"
                 Equal   -> "=="
                 NotEq   -> "!="
                 LessEq  -> "<="
                 GreatEq -> ">="
                 Like    -> "=~"

  instance Show Query where
    show (Query ss s p) = concat ["query ", show ss, " ", show s , " ", show p]
  
  instance Show Source where
    show = \case (Single s rec)     -> show_rec rec (show s)
                 (Join j s sel rec) -> show_rec rec $
                                        concat [show j, " join ", show s, " on "
                                               , show sel]
      where
        show_rec rec | rec       = ("recursive " ++)
                     | otherwise = id
  
  instance Show Join where
    show = \case Inner -> "inner"
                 Outer -> "outer"
                 Left  -> "left"
                 Right -> "right"
                 Full  -> "full"
  
  instance Show Predicate where
    show = const "Predicate"
  
  instance Show (Maybe Predicate) where
    show = maybe "Nothing" show
