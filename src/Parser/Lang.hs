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
    fsql_dir, fsql_expr, fsql_joinType, fsql_recursive, fsql_selection, fsql_selections,
    reserved, whiteSpace
  ) where
  
  import Prelude hiding (Either(..))
  
  import Data.Functor         (($>))
  
  import Text.Parsec          ((<|>), (<?>), between, many1, optionMaybe, try, unexpected)
  import Text.Parsec.Char     (anyChar, char, noneOf, oneOf)
  import Text.Parsec.Language (emptyDef)
  import Text.Parsec.Expr     (Operator(Prefix, Infix), Assoc(AssocRight, AssocNone)
                              , buildExpressionParser                               )
  import qualified Text.Parsec.Token as Token (LanguageDef, TokenParser
                                              , commaSep1, lexeme, parens, whiteSpace
                                              , reserved, reservedOp, reservedOpNames
                                              , makeTokenParser                       )
  
  import Expr.Untyped (Expr(..), Op(..), BinOp(..))
  import Query        (Selection(..), JoinType(..))
  import Parser.Base  (Parser, OperatorTable)
  
  
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
  
  
  fsql_dir :: Parser FilePath
  fsql_dir = lexeme (
              string (rawString "\"\\") -- Quoted.
              <|> rawString "\"\\ "     -- Or not.
             )
          <?> "directory path"
  
  
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
             
             , [ Prefix (reservedOp "!" $> Op (:!:)) ]
             
             , [ Infix (reservedOp "&&" $> BinOp (:&&:)) AssocRight ]
             , [ Infix (reservedOp "||" $> BinOp (:||:)) AssocRight ] ]
  
  fsql_value :: Parser String
  fsql_value = lexeme (
                string (rawString' "\"\\" (oneOf "\"\\") (noneOf "\"\\"))
                <|> rawString' "\"\\ !<>&|=()"  -- avoid ambiguity with operators.
                               (unexpected "escaped character") -- disallow backslashes.
                               (unexpected "escaped character")
               )
             <?> "literal value"
  
  fsql_expr :: Parser Expr
  fsql_expr = buildExpressionParser fsql_ops (
                parens fsql_expr
                <|> (Sel <$> fsql_selection)
                <|> (Val <$> fsql_value)
                <?> "expression"
              )
  
  
  -- fsql_langDef ------------------------------------------------------------------------
  fsql_langDef :: Token.LanguageDef u
  fsql_langDef = emptyDef { Token.reservedOpNames = [ "<", ">", "<=", ">=", "=="
                                                    , "!=", "=~", "&&", "||", "!" ] }
  
  fsql_lexer :: Token.TokenParser u
  fsql_lexer = Token.makeTokenParser fsql_langDef
  
  
  commaSep1  :: Parser a -> Parser [a]
  parens     :: Parser a -> Parser a
  lexeme     :: Parser a -> Parser a
  reserved   :: String -> Parser ()
  reservedOp :: String -> Parser ()
  whiteSpace :: Parser ()
  
  commaSep1  = Token.commaSep1  fsql_lexer
  parens     = Token.parens     fsql_lexer
  lexeme     = Token.lexeme     fsql_lexer
  reserved   = Token.reserved   fsql_lexer
  reservedOp = Token.reservedOp fsql_lexer
  whiteSpace = Token.whiteSpace fsql_lexer
  
  
  string :: Parser String -> Parser String
  string = between (char '"') (char '"' <?> "end of string")
  
  rawString :: String         -- Characters that must be escaped.
            -> Parser String
  rawString s = many1 (
                  noneOf s
                  <|> (char '\\' >> anyChar <?> "escaped char")
                )
  
  rawString' :: String        -- Characters that must be escaped
             -> Parser Char   -- Parser for escape characters.
             -> Parser Char   -- Parser for escape characters to be kept escaped.
             -> Parser String
  rawString' s e e' = concat <$> many1 (
                        (return <$> noneOf s)
                        <|> try (char '\\' >> return <$> e  <?> "escaped char")
                        <|> try (char '\\' >> escape <$> e' <?> "escaped char")
                      )
    where
      escape c = ['\\', c]
  -- -------------------------------------------------------------------------------------
