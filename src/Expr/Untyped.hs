{- FSQL : Expr/Untyped.hs -- Untyped expression data types and functions
 -
 - Copyright (C) 2015 gahag
 - All rights reserved.
 -
 - This software may be modified and distributed under the terms
 - of the BSD license. See the LICENSE file for details.
 -}

module Expr.Untyped (
    Expr(..), Op(..), BinOp(..)
  ) where
  
  import Query  (Selection(..))
  
  
  data Expr = Sel Selection
            | Val String
            | Op Op Expr
            | BinOp BinOp Expr Expr
  
  data Op = (:!:) -- Expr -> Expr
  
  data BinOp = (:&&:)  -- Expr -> Expr  -> Expr
             | (:||:)  -- Expr -> Expr  -> Expr
             | (:>:)   -- a    -> a     -> Expr
             | (:<:)   -- a    -> a     -> Expr
             | (:>=:)  -- a    -> a     -> Expr
             | (:<=:)  -- a    -> a     -> Expr
             | (:==:)  -- a    -> a     -> Expr
             | (:/=:)  -- a    -> a     -> Expr
             | (:=~:)  -- a    -> Regex -> Expr
  
  
  instance Show Expr where
    show (Sel s) = show s
    show (Val v) = v
    show (Op op e) = "(" ++ show op ++ show e ++ ")"
    show (BinOp op e e') = "(" ++ show e ++ " " ++ show op ++ " " ++ show e' ++ ")"
  
  instance Show Op where
    show (:!:) = "!"
  
  instance Show BinOp where
    show (:&&:) = "&&"
    show (:||:) = "||"
    show (:>:)  = ">"
    show (:<:)  = "<"
    show (:>=:) = ">="
    show (:<=:) = "<="
    show (:==:) = "=="
    show (:/=:) = "/="
    show (:=~:) = "=~"
