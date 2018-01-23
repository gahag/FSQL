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
    ParseError,
    fsql_parse
  ) where
  
  import Control.Monad.Except (ExceptT(..))
  
  import Text.Parsec        ((<?>), between, eof, optionMaybe, parse)
  import Text.Parsec.Error  (ParseError)
  
  import Query        (Query(..), Source(..), Predicate, Selection)
  import Expr         (expr_to_Pred)
  import Parser.Base  (Parser)
  import Parser.Lang  (fsql_expr, fsql_ident, fsql_joinType, fsql_recursive
                      , fsql_selection, fsql_selections, reserved, whiteSpace)
  
  
  fsql_parse :: (Monad m) => String  -- Source name
                          -> String  -- Input
                          -> ExceptT ParseError m Query
  fsql_parse name = ExceptT . return . parse (between whiteSpace eof fsql) name
  
  
  fsql :: Parser Query
  fsql = Query
      <$> fsql_select
      <*> fsql_source
      <*> optionMaybe fsql_where
      <?> "query statement"
  
  -- Parse the selections of the query.
  fsql_select :: Parser [Selection]
  fsql_select = reserved "select"
              *> fsql_selections
             <?> "select statement"
  
  -- Parse the source of the query.
  fsql_source :: Parser Source
  fsql_source = (\ rec fp -> \case Nothing            -> Source rec fp
                                   Just (j, fp', sel) -> Join rec j (fp, fp') sel)
             <$> fsql_recursive  -- recursive keyword.
             <*  reserved "from" -- from keyword.
             <*> fsql_ident      -- source directory.
             <*> optionMaybe (   -- optional join:
                  (,,) <$> fsql_joinType   -- join type keyword (inner, outer, etc).
                       <*  reserved "join" -- join keyword.
                       <*> fsql_ident      -- source directory.
                       <*  reserved "on"   -- on keyword.
                       <*> fsql_selection  -- join selection (join by name, date, etc).
                       <?> "join statement"
                 )
             <?> "from statement"
  
  -- Parse the predicate of the query
  fsql_where :: Parser Predicate
  fsql_where = expr_to_Pred
            <$>(reserved "where"
             *> fsql_expr)
            <?> "where statement"
