{- FSQL : Parser.hs -- Main parser functions
 -
 - Copyright (C) 2015 gahag
 - All rights reserved.
 -
 - This software may be modified and distributed under the terms
 - of the BSD license. See the LICENSE file for details.
 -}

{-# LANGUAGE LambdaCase #-}

module Parser (
    parse_fsql
  ) where
  
  import Text.Parsec  ((<?>), between, eof, optionMaybe, parse)
  
  import Query        (Query(..), Source(..))
  import Expr         (expr_to_Pred)
  import Parser.Expr  (fsql_expr)
  import Parser.Lang  (fsql_ident, fsql_joinType, fsql_recursive, fsql_selection
                      , fsql_selections, reserved, whiteSpace)
  
  
  
  parse_fsql = parse fsql_parser
  
  fsql_parser = fsql `between'` whiteSpace $ eof
    where between' p open close = between open close p
  
  
  fsql_select = reserved "select"
            *>  fsql_selections
            <?> "select statement"
  
  fsql_source = (\ rec s -> \case Just join -> join (Source s rec)
                                  Nothing   -> Source s rec)
                  <$> fsql_recursive
                  <*  reserved "from"
                  <*> fsql_ident -- source (directory)
                  <*> optionMaybe fsql_join
                  <?> "from statement"
  
  fsql_join = (\ j s' sel s -> Join j (s, Source s' $ recurse s) sel) -- See fsql_source.
           -- Swap s and s' because s' is the first source, and s is the second.
                <$> fsql_joinType   -- join type
                <*  reserved "join"
                <*> fsql_ident      -- source (directory)
                <*  reserved "on"
                <*> fsql_selection  -- join selection
                <?> "join statement"
  
  fsql_where = expr_to_Pred
                <$>(reserved "where"
                *>  fsql_expr)
                <?> "where statement"
  
  
  fsql = Query
          <$> fsql_select
          <*> fsql_source
          <*> optionMaybe fsql_where
          <?> "query statement"
