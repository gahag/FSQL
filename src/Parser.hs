{- FSQL : Parser.hs -- Main parser functions
 -
 - Copyright (C) 2015 gahag
 - All rights reserved.
 -
 - This software may be modified and distributed under the terms
 - of the BSD license. See the LICENSE file for details.
 -}

{-# LANGUAGE LambdaCase #-}

module Parser where
  
  import Control.Applicative  ((<$>), (<*>), (*>), (<*))
  
  import Text.Parsec      ((<|>), (<?>), between, eof, optionMaybe, parse)
  import Text.Parsec.Expr (buildExpressionParser)
  
  import Query        (Query(..), Source(..))
  import Expr         (expr_to_Pred)
  import Parser.Expr  (fsql_expr)
  import Parser.Lang  (fsql_ident, fsql_joinType, fsql_selection, parens
                      , reserved, whiteSpace)
  
  
  
  parse_fsql = parse fsql_parser []
  
  fsql_parser = fsql `between'` whiteSpace $ eof
    where between' p open close = between open close p
  
  
  fsql_select = reserved "select"
            *>  fsql_selection
            <?> "select statement"
  
  fsql_source = (\ s -> maybe (Single s) ($ s))
                  <$>(reserved "from"
                  *>  fsql_ident)
                  <*> optionMaybe fsql_join
  
  fsql_join = (\ j s sel s' -> Join j (s', s) sel)
                <$> fsql_joinType     -- join type
                <*  reserved "join"
                <*> fsql_ident        -- source
                <*  reserved "on"
                <*> fsql_selection    -- join selection
                <?> "join statement"
  
  fsql_where = expr_to_Pred
                <$>(reserved "where"
                *>  fsql_expr)
  

  fsql = Query
          <$> fsql_select
          <*> fsql_source
          <*> optionMaybe fsql_where
