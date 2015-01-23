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
  import Data.Functor   ((<$>), ($>))
  import Text.Read      (readMaybe)
  
  import Text.Parsec          ((<|>), (<?>), noneOf, optionMaybe)
  import Text.Parsec.Language (emptyDef)
  import Text.Parsec.Expr     (Operator(Prefix, Infix), Assoc(AssocRight))
  import qualified Text.Parsec.Token as Token (commaSep1, identLetter
                                              , identStart, identifier
                                              , makeTokenParser, parens
                                              , reserved, reservedNames
                                              , reservedOp, reservedOpNames
                                              , stringLiteral, whiteSpace   )
  
  import Query (Selection(..), Join(..))
  import Expr  (Expr(..), Value(..), BooleanOp(..), RelationalOp(..))
  
  
  
  fsql_langDef = emptyDef {
      Token.identStart      = noneOf fsql_ident_invalidCs
    , Token.identLetter     = noneOf fsql_ident_invalidCs
    
    , Token.reservedNames   = [ "select", "from", "inner", "outer", "left"
                              , "right", "full", "join", "on", "where"
                              , "name", "date", "size"                    ]
    
    , Token.reservedOpNames = [ "<", ">", "<=", ">=", "==", "!=", "&&", "||"
                              , "!"                                         ]
  }  
  
  -- fsql_ident_invalidCs : all the operator chars, quotation chars, parenthesis
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
  
  fsql_selection =  (reserved "name" $> Name)
                <|> (reserved "date" $> Date)
                <|> (reserved "size" $> Size)
                <?> "selection identifier (name|date|size)"
  
  fsql_selections = commaSep1 fsql_selection
  
  fsql_val s = case s of
                Name -> return . RawVal
                Date -> parseVal DayVal
                Size -> parseVal SizeVal
    where
      parseVal f = maybe (fail $ "invalid " ++ show s) (return . f) . readMaybe
  
  fsql_recursive = (/= Nothing)
                    <$> optionMaybe (reserved "recursive")
  
  fsql_joinType =  (reserved "inner" $> Inner)
               <|> (reserved "outer" $> Outer)
               <|> (reserved "left"  $> Left )
               <|> (reserved "right" $> Right)
               <|> (reserved "full"  $> Full )
               <?> "join type (inner|outer|left|right|full)"
  
  
  fsql_boolOps = [ [Prefix (reservedOp "!"  $> Not)]
                 , [Infix  (reservedOp "&&" $> BoolOp And) AssocRight]
                 , [Infix  (reservedOp "||" $> BoolOp Or ) AssocRight] ]
  
  fsql_relOp = foldr ((<|>) . uncurry ($>) . (first reservedOp)) mzero
    [ ("==", Equal  )
    , ("!=", NotEq  )
    , ("<=", LessEq )
    , (">=", GreatEq)
    , ("<" , Less   )
    , (">" , Greater) ]
    <?> "relational operator (== | != | <= | >= | < | >)"
