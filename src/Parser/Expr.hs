{- FSQL : Parser/Expr.hs -- Expression parsing functions
 -
 - Copyright (C) 2015 gahag
 - All rights reserved.
 -
 - This software may be modified and distributed under the terms
 - of the BSD license. See the LICENSE file for details.
 -}

module Parser.Expr where 
  
  import Text.Parsec      ((<|>), (<?>))
  import Text.Parsec.Expr (Operator(Prefix, Infix), Assoc(AssocRight)
                          , buildExpressionParser)
  
  import Expr         (Expr(RelOp), flip_relOp)
  import Parser.Lang  (fsql_andOp, fsql_ident, fsql_notOp, fsql_orOp, fsql_relOp
                      ,fsql_selection, fsql_val, parens)
  
  
  
  fsql_ops = [ [Prefix fsql_notOp            ]
             , [Infix  fsql_andOp AssocRight ]
             , [Infix  fsql_orOp  AssocRight ] ]
  
  fsql_relOperation =
    do sel <- fsql_selection
       op  <- fsql_relOp
       val <- fsql_ident >>= fsql_val sel
       return (RelOp op sel val)
    <|>
    do val' <- fsql_ident
       op   <- fsql_relOp
       sel  <- fsql_selection
       val  <- fsql_val sel val'
       return (RelOp (flip_relOp op) sel val)       
    <?> "relational expression"
  
  
  fsql_expr = buildExpressionParser fsql_ops
               (parens fsql_expr
                <|> fsql_relOperation
                <?> "expression"      )
