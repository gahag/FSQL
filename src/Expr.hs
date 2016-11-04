{- FSQL : Expr.hs -- Expression data types and functions
 -
 - Copyright (C) 2015 gahag
 - All rights reserved.
 -
 - This software may be modified and distributed under the terms
 - of the BSD license. See the LICENSE file for details.
 -}

{-# LANGUAGE LambdaCase, TupleSections #-}
{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module Expr (
    Expr(..), Atom(..), Value(..), Op(..),
    TypeError(..), typecheck,
    expr_to_Pred
  ) where
  
  import Control.Arrow        ((***))
  import Control.Monad.Except (throwError)
  import Text.Read            (readMaybe)
  
  import Text.Regex.TDFA  (Regex, makeRegexM, match)
  
  import Query    (Selection(..), Predicate)
  import FileInfo (Day, FileSize, name, date, size)
  
  
  
  data Expr = Atom Atom
            | Not Expr
            | Op Op Expr Expr
  
  data Atom = Sel Selection
            | Val Value
  
  data Value = UnparsedVal String -- Build Expr Values with `UnparsedVal`, and
             | StrVal   String    -- `typecheck` will parse it into the
             | RegexVal Regex     -- appropriate value type.
             | DayVal   Day
             | SizeVal  FileSize
  
  data Op = And
          | Or
          | Less
          | Greater
          | Equal
          | NotEq
          | LessEq
          | GreatEq
          | Like
  
  data TypeError = InvalidValue   String String -- Invalid Value -> Value type
                 | UnexpectedType String String -- Unexpected    -> Expected
  
  type TypeChecked = Either TypeError
  
  
  instance Show Atom where
    show = \case Sel s -> show s
                 Val v -> show v
  
  instance Show Value where
    show = \case UnparsedVal s -> s
                 StrVal      s -> s
                 DayVal      d -> show d
                 SizeVal     s -> show s
  
  instance Show TypeError where
    show =
      \case InvalidValue   u e -> concat ["invalid ", e, ": ", u]
            UnexpectedType u e -> concat ["unexpected ", u, "\nexpected ", e]
  
  
  val      = Atom . Val
  strVal   = val . StrVal
  regexVal = val . RegexVal
  dayVal   = val . DayVal
  sizeVal  = val . SizeVal
  
  quote s = "\"" ++ show s ++ "\""  
  invalid_val = InvalidValue . quote
  unexpected  = UnexpectedType
  x `expecting` s = throwError (x s)
  
  
  
  -- typecheck -----------------------------------------------------------------
  typecheck :: Expr -> TypeChecked Expr
  
  typecheck (Atom a) = unexpected (quote a) `expecting` "expression"
  
  typecheck (Not x) = Not <$> typecheck x
  
  typecheck (Op op x x') = (Op op `uncurry`)
                        <$> case op of
                              And     -> bool'bool'
                              Or      -> bool'bool'
                              Less    -> a'a'
                              Greater -> a'a'
                              Equal   -> a'a'
                              NotEq   -> a'a'
                              LessEq  -> a'a'
                              GreatEq -> a'a'
                              Like    -> name'regexVal'
    where
      bool'bool' = (,) <$> typecheck x <*> typecheck x'
      
      name'regexVal'
        | (Atom a, Atom a') <- (x, x')
          = case (a, a') of
              (Sel Name, Val (UnparsedVal s))
                -> maybe (InvalidValue (show s) `expecting` "regex")
                        (return . (x,) . regexVal)
                      $ makeRegexM s -- TODO grab error message from `makeRegexM`.

              (Sel Name, atom) -> unexpected (quote atom) `expecting` "value"
              (atom, _) -> unexpected (quote atom) `expecting` "`name`"
        | otherwise = unexpected "expression" `expecting`
                                                "selection identifier or value"
      
      a'a'
        | (Atom a, Atom a') <- (x, x')
          = case (a, a') of
              (Sel s, Val v) -> (x,)  <$> parseVal s v
              (Val v, Sel s) -> (,x') <$> parseVal s v
              (Sel _, atom) -> unexpected (quote atom) `expecting` "value"
              (Val _, atom) -> unexpected (quote atom) `expecting`
                                                        "selection identifier"
        | otherwise = unexpected "expression" `expecting`
                                                "selection identifier or value"
        where
          parseVal sel (UnparsedVal str) =
            let parse f   = maybe parse_err (return . f) . readMaybe
                parse_err = invalid_val str `expecting` (show sel ++ " value")
            in
              case sel of
                Name -> return (strVal str)
                Date -> parse dayVal  str
                Size -> parse sizeVal str
          
          parseVal _ _ = error "Expr.typecheck.parseVal: not UnparsedVal"
  -- ---------------------------------------------------------------------------
  
  
  -- expr_to_Pred --------------------------------------------------------------
  expr_to_Pred :: Expr -> Predicate
  
  expr_to_Pred (Atom _) = error "Expr.expr_to_Pred: invalid expression"
  
  expr_to_Pred (Not x) = not . expr_to_Pred x
  
  expr_to_Pred (Op op x x') =
    case op of
      And -> (&&) `bool'bool'` x $ x'
      Or  -> (||) `bool'bool'` x $ x'
     
      Less    | (Atom a, Atom a') <- (x, x') -> (<)  `ord'ord'`  a $ a'
      Greater | (Atom a, Atom a') <- (x, x') -> (>)  `ord'ord'`  a $ a'
      Equal   | (Atom a, Atom a') <- (x, x') -> (==) `ord'ord'`  a $ a'
      NotEq   | (Atom a, Atom a') <- (x, x') -> (/=) `ord'ord'`  a $ a'
      LessEq  | (Atom a, Atom a') <- (x, x') -> (<=) `ord'ord'`  a $ a'
      GreatEq | (Atom a, Atom a') <- (x, x') -> (>=) `ord'ord'`  a $ a'
      
      Like    | (Atom a, Atom a') <- (x, x') -> (flip match) `name'regex'` a $ a'
     
      _ -> error invalid_expr
    where      
      bool'bool' op x x' = \ fi -> expr_to_Pred x fi `op` expr_to_Pred x' fi
      
      name'regex' op (Sel Name) (Val (RegexVal r)) = (`op` r) . name
      name'regex' _ _ _ = error invalid_expr
      
      ord'ord' :: (forall a. (Ord a) => a -> a -> Bool)
               -> Atom -> Atom -> Predicate
      ord'ord' op a a'
        | (Sel s, Val v) <- (a, a') = op      `on` s $ v
        | (Val v, Sel s) <- (a, a') = flip op `on` s $ v
        | otherwise = error invalid_expr
        where
          on :: (forall a. (Ord a) => a -> a -> Bool)
             -> Selection -> Value -> Predicate
          on op s v =
            let op_on selector = uncurry (flip op) . curry selector v
            in case s of
                Name -> op_on (fromStrVal  *** name)
                Date -> op_on (fromDayVal  *** date)
                Size -> op_on (fromSizeVal *** size)

          
          fromStrVal  = \case StrVal  s -> s; _ -> invalid_value "fromRaw"
          fromDayVal  = \case DayVal  d -> d; _ -> invalid_value "fromDay"
          fromSizeVal = \case SizeVal s -> s; _ -> invalid_value "fromSize"
          
          invalid_value fn = error $ "Expr." ++ fn ++ ": invalid value."
      
      
      invalid_expr = "Expr.expr_to_Pred: invalid expression"
  -- ---------------------------------------------------------------------------
