{- FSQL : Expr.hs -- Expression data types and functions
 -
 - Copyright (C) 2015 gahag
 - All rights reserved.
 -
 - This software may be modified and distributed under the terms
 - of the BSD license. See the LICENSE file for details.
 -}

{-# LANGUAGE LambdaCase #-}

module Expr where
  
  import Control.Arrow        ((***), (&&&))
  
  import Query    (Selection(..), Predicate)
  import FileInfo (Day, FileOffset, FileInfo, name, date, size)
  
  
  
  data Expr = Not Expr
            | BoolOp BooleanOp    Expr     Expr
            | RelOp  RelationalOp Selection Value
  
  data Value = RawVal  String
             | DayVal  Day
             | SizeVal FileOffset
  
  data BooleanOp = And
                 | Or
  
  data RelationalOp = Less
                    | Greater
                    | Equal
                    | NotEq
                    | LessEq
                    | GreatEq
  
  
  instance Show BooleanOp where
    show = \case And -> "&&"
                 Or  -> "||"
  
  instance Show RelationalOp where
    show = \case Less    -> "<"
                 Greater -> ">"
                 Equal   -> "=="
                 NotEq   -> "!="
                 LessEq  -> "<="
                 GreatEq -> ">="
  
  
  
  -- expr_to_Pred --------------------------------------------------------------
  expr_to_Pred :: Expr -> Predicate
  
  expr_to_Pred (Not x) = not . expr_to_Pred x
  
  expr_to_Pred (BoolOp op x x') = uncurry (boolOp_to_op op)
                                  . (expr_to_Pred x &&& expr_to_Pred x')
  
  expr_to_Pred (RelOp op s v) = s `operator` v
    where
      operator :: Selection -> Value -> Predicate
      operator = \case Name -> op_on (fromRaw  *** name)
                       Date -> op_on (fromDay  *** date)
                       Size -> op_on (fromSize *** size)
      
      op_on selector v = uncurry (flip $ relOp_to_op op) . (curry selector) v
  -- ---------------------------------------------------------------------------
  
  
  boolOp_to_op :: BooleanOp -> (Bool -> Bool -> Bool)
  boolOp_to_op = \case And -> (&&)
                       Or  -> (||)
  
  relOp_to_op :: (Ord a) => RelationalOp -> (a -> a -> Bool)
  relOp_to_op = \case Less    -> (<)
                      Greater -> (>) 
                      Equal   -> (==)
                      NotEq   -> (/=)
                      LessEq  -> (<=)
                      GreatEq -> (>=)
  
  flip_relOp :: RelationalOp -> RelationalOp
  flip_relOp = \case Less    -> Greater
                     Greater -> Less
                     Equal   -> Equal
                     NotEq   -> NotEq
                     LessEq  -> GreatEq
                     GreatEq -> LessEq
  
  
  fromRaw  :: Value -> String
  fromDay  :: Value -> Day
  fromSize :: Value -> FileOffset
  
  fromRaw  = \case RawVal  r -> r; _ -> error "Expr.fromRaw: Invalid Value."
  fromDay  = \case DayVal  d -> d; _ -> error "Expr.fromDay: Invalid Value."
  fromSize = \case SizeVal s -> s; _ -> error "Expr.fromSize: Invalid Value."
