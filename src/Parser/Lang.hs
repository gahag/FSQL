{- FSQL : Parser/Lang.hs -- Language definition, lexer and standard parser
                            functions
 -
 - Copyright (C) 2015 gahag
 - All rights reserved.
 -
 - This software may be modified and distributed under the terms
 - of the BSD license. See the LICENSE file for details.
 -}

module Parser.Lang (
    fsql_expr, fsql_ident, fsql_joinType, fsql_recursive, fsql_selection, fsql_selections,
    reserved, whiteSpace
  ) where
  
  import Prelude hiding (Either(..))
  
  import Data.Functor (($>))
  
  import Text.Parsec          ((<|>), (<?>), noneOf, optionMaybe)
  import Text.Parsec.Language (emptyDef)
  import Text.Parsec.Expr     (Operator(Prefix, Infix), Assoc(AssocRight, AssocNone)
                              , buildExpressionParser                               )
  import qualified Text.Parsec.Token as Token (LanguageDef, TokenParser
                                              , commaSep1, identLetter, identStart
                                              , identifier, makeTokenParser, parens
                                              , reserved, reservedOp, reservedOpNames
                                              , stringLiteral, whiteSpace             )
  
  import Expr.Untyped (Expr(..), Op(..), BinOp(..))
  import Query        (Selection(..), JoinType(..))
  import Parser.Base  (Parser, OperatorTable)
  
  
  
  fsql_ident :: Parser String
  fsql_ident = ident <|> string
            <?> "identifier or string"
  
  
  fsql_selection :: Parser Selection
  fsql_selection =  (reserved "name" $> Name)
                <|> (reserved "date" $> Date)
                <|> (reserved "size" $> Size)
                <?> "selection identifier (name|date|size)"
  
  fsql_selections :: Parser [Selection]
  fsql_selections = commaSep1 fsql_selection
                  <?> "one or more selections"
  
  
  fsql_recursive :: Parser Bool
  fsql_recursive = (/= Nothing)
                    <$> optionMaybe (reserved "recursive")
                    <?> "keyword `recursive`"
  
  fsql_joinType :: Parser JoinType
  fsql_joinType =  (reserved "inner" $> Inner)
               <|> (reserved "outer" $> Outer)
               <|> (reserved "left"  $> Left )
               <|> (reserved "right" $> Right)
               <|> (reserved "full"  $> Full )
               <?> "join type (inner|outer|left|right|full)"
  
  
  fsql_ops :: OperatorTable Expr
  fsql_ops = [ [ Infix (reservedOp "==" $> BinOp (:==:)) AssocNone
               , Infix (reservedOp "!=" $> BinOp (:/=:)) AssocNone
               , Infix (reservedOp "<=" $> BinOp (:<=:)) AssocNone
               , Infix (reservedOp ">=" $> BinOp (:>=:)) AssocNone
               , Infix (reservedOp "<"  $> BinOp (:<:))  AssocNone
               , Infix (reservedOp ">"  $> BinOp (:>:))  AssocNone
               , Infix (reservedOp "=~" $> BinOp (:=~:)) AssocNone ]
             
             , [ Prefix (reservedOp "!"  $> Op (:!:)) ]
             
             , [ Infix (reservedOp "&&" $> BinOp (:&&:)) AssocRight ]
             , [ Infix (reservedOp "||" $> BinOp (:||:)) AssocRight ] ]
  
  fsql_expr :: Parser Expr
  fsql_expr = buildExpressionParser fsql_ops (
                parens fsql_expr
                <|>(Sel <$> fsql_selection)
                <|>(Val <$> fsql_ident)
                <?> "expression"
              )
  
  
    -- fsql_langDef ------------------------------------------------------------------------
  fsql_langDef :: Token.LanguageDef u
  fsql_langDef = emptyDef {
      Token.identStart      = noneOf fsql_ident_invalidCs
    , Token.identLetter     = noneOf fsql_ident_invalidCs
    
    , Token.reservedOpNames = [ "<", ">", "<=", ">=", "==", "!=", "=~"
                              , "&&", "||", "!"                       ]
  }
  
  -- fsql_ident_invalidCs : all the operator chars, quotation chars, parenthesis,
  -- whitespace and comma (comma is used as delimiter in `fsql_selections`).
  fsql_ident_invalidCs :: String
  fsql_ident_invalidCs = "\" '!<>&|=(),"
  -- -------------------------------------------------------------------------------------
  
  
  -- fsql_lexer --------------------------------------------------------------------------
  fsql_lexer :: Token.TokenParser u
  fsql_lexer = Token.makeTokenParser fsql_langDef
  
  
  commaSep1  :: Parser a -> Parser [a]
  ident      :: Parser String
  parens     :: Parser a -> Parser a
  reserved   :: String -> Parser ()
  reservedOp :: String -> Parser ()
  string     :: Parser String
  whiteSpace :: Parser ()
  
  commaSep1  = Token.commaSep1     fsql_lexer
  ident      = Token.identifier    fsql_lexer
  parens     = Token.parens        fsql_lexer
  reserved   = Token.reserved      fsql_lexer
  reservedOp = Token.reservedOp    fsql_lexer
  string     = Token.stringLiteral fsql_lexer
  whiteSpace = Token.whiteSpace    fsql_lexer
  -- -------------------------------------------------------------------------------------
