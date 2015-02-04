{- FSQL : Parser/Expr.hs -- Expression parsing functions
 -
 - Copyright (C) 2015 gahag
 - All rights reserved.
 -
 - This software may be modified and distributed under the terms
 - of the BSD license. See the LICENSE file for details.
 -}

module Parser.Expr (
    fsql_expr, typecheck
  ) where 
  
  import Control.Applicative  ((<$>))
  
  import Text.Parsec      (Parsec, (<|>), (<?>))
  import Text.Parsec.Expr (buildExpressionParser)
  
  import Expr         (Expr(Atom), Atom(..), Value(..), TypeError(..), typecheck)
  import Parser.Lang  (fsql_ident, fsql_ops, fsql_selection, parens)
  
  
  
  fsql_expr = fsql_untypedExpr
          >>= fsql_typecheck
  
  fsql_untypedExpr = buildExpressionParser fsql_ops (
                      parens fsql_untypedExpr
                      <|>(Atom . Sel <$> fsql_selection)
                      <|>(Atom . Val . UnparsedVal <$> fsql_ident)
                      <?> "expression"
                     )
  
  fsql_typecheck :: Expr -> Parsec String u Expr
  fsql_typecheck = either (fail . show) return . typecheck
