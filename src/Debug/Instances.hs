{- FSQL : Debug/Instances.hs -- Type instances for debuging
 -
 - Copyright (C) 2015 gahag
 - All rights reserved.
 -
 - This software may be modified and distributed under the terms
 - of the BSD license.  See the LICENSE file for details.
 -}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}

module Debug.Instances where
  
  import Prelude hiding (Either(..))
  
  import Query  (Query(..), Source(..), JoinType(..), Predicate)
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
    show = \case (Source rec s)     -> show_rec rec (show s)
                 (Join rec j s sel) -> show_rec rec $
                                        concat [show j, " join ", show s, " on "
                                               , show sel]
      where
        show_rec rec | rec       = ("recursive " ++)
                     | otherwise = id
  
  instance Show JoinType where
    show = \case Inner -> "inner"
                 Outer -> "outer"
                 Left  -> "left"
                 Right -> "right"
                 Full  -> "full"
  
  instance {-# OVERLAPS #-} Show Predicate where
    show = const "Predicate"
  
  instance {-# OVERLAPS #-} Show (Maybe Predicate) where
    show = maybe "Nothing" show
