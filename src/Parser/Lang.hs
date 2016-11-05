{- FSQL : Parser/Lang.hs -- Language definition, lexer and standard parser
                            functions
 -
 - Copyright (C) 2015 gahag
 - All rights reserved.
 -
 - This software may be modified and distributed under the terms
 - of the BSD license. See the LICENSE file for details.
 -}

module Parser.Lang where
  
  import Prelude hiding (Either(..))
  
  import Control.Arrow  (first)
  import Control.Monad  (mzero)
  import Data.Functor   (($>))
  
  import Text.Parsec          ((<|>), (<?>), noneOf, optionMaybe)
  import Text.Parsec.Language (emptyDef)
  import Text.Parsec.Expr     (Operator(Prefix, Infix)
                              , Assoc(AssocRight, AssocNone))
  import qualified Text.Parsec.Token as Token (commaSep1, identLetter
                                              , identStart, identifier
                                              , makeTokenParser, parens
                                              , reserved, reservedOp
                                              , reservedOpNames, stringLiteral
                                              , whiteSpace                    )
  
  import Query (Selection(..), JoinType(..))
  import Expr  (Expr(Not, Op), Op(..))
  
  
  
  fsql_langDef = emptyDef {
      Token.identStart      = noneOf fsql_ident_invalidCs
    , Token.identLetter     = noneOf fsql_ident_invalidCs
    
    , Token.reservedOpNames = [ "<", ">", "<=", ">=", "==", "!=", "=~"
                              , "&&", "||", "!"                       ]
  }  
  
  -- fsql_ident_invalidCs : all the operator chars, quotation chars, parenthesis,
  -- whitespace and comma (comma is used as delimiter in `fsql_selections`).
  fsql_ident_invalidCs = "\" '!<>&|=(),"

  
  fsql_lexer = Token.makeTokenParser fsql_langDef
  
  commaSep1  = Token.commaSep1     fsql_lexer
  ident      = Token.identifier    fsql_lexer
  parens     = Token.parens        fsql_lexer
  reserved   = Token.reserved      fsql_lexer
  reservedOp = Token.reservedOp    fsql_lexer
  string     = Token.stringLiteral fsql_lexer
  whiteSpace = Token.whiteSpace    fsql_lexer
  
  
  fsql_ident = ident <|> string
            <?> "identifier or string"
  
  
  fsql_selection =  (reserved "name" $> Name)
                <|> (reserved "date" $> Date)
                <|> (reserved "size" $> Size)
                <?> "selection identifier (name|date|size)"
  
  fsql_selections = commaSep1 fsql_selection
                  <?> "one or more selections"
  
  
  fsql_recursive = (/= Nothing)
                    <$> optionMaybe (reserved "recursive")
                    <?> "keyword `recursive`"
  
  fsql_joinType =  (reserved "inner" $> Inner)
               <|> (reserved "outer" $> Outer)
               <|> (reserved "left"  $> Left )
               <|> (reserved "right" $> Right)
               <|> (reserved "full"  $> Full )
               <?> "join type (inner|outer|left|right|full)"
  
  
  fsql_ops = [ [ Infix (reservedOp "==" $> Op Equal  ) AssocNone
               , Infix (reservedOp "!=" $> Op NotEq  ) AssocNone
               , Infix (reservedOp "<=" $> Op LessEq ) AssocNone
               , Infix (reservedOp ">=" $> Op GreatEq) AssocNone
               , Infix (reservedOp "<"  $> Op Less   ) AssocNone
               , Infix (reservedOp ">"  $> Op Greater) AssocNone
               , Infix (reservedOp "=~" $> Op Like   ) AssocNone ]
             
             , [ Prefix (reservedOp "!"  $> Not) ]
             
             , [ Infix (reservedOp "&&" $> Op And) AssocRight ]
             , [ Infix (reservedOp "||" $> Op Or ) AssocRight ] ]
