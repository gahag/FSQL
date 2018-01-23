{- FSQL : Parser/Base.hs -- Basic parser definitions
 -
 - Copyright (C) 2015 gahag
 - All rights reserved.
 -
 - This software may be modified and distributed under the terms
 - of the BSD license. See the LICENSE file for details.
 -}

module Parser.Base (
    Parser, OperatorTable
  ) where
  
  import Data.Functor.Identity  (Identity)
  
  import Text.Parsec                      (Parsec)
  import qualified Text.Parsec.Expr as E  (OperatorTable)
  
  
  type Parser = Parsec String ()  -- String as stream type, no user state.
  
  type OperatorTable a = E.OperatorTable String () Identity a
