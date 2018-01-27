{- FSQL : Expr/Untyped.hs -- Untyped expression data types and functions
 -
 - Copyright (C) 2015 gahag
 - All rights reserved.
 -
 - This software may be modified and distributed under the terms
 - of the BSD license. See the LICENSE file for details.
 -}

{-# LANGUAGE FlexibleContexts, FlexibleInstances, RankNTypes #-}

module Expr.TypeChecker (
    TypeError(..),
    typecheck
  ) where
  
  import Control.Monad.Except (ExceptT(..), throwError, withExceptT)
  import Text.Read            (readMaybe)
  
  import qualified Expr.Untyped as U (Expr(..), Op(..), BinOp(..))
  import           Expr.Typed        (Sel(..), Expr(..))
  import           FileInfo          (Day, FileSize)
  import           Query             (Selection(..))
  import           Regex             (Regex, makeRegex)
  
  
  -- TypeError ---------------------------------------------------------------------------
  data TypeError = ParseError String -- Expected type
                              String -- Literal
                 | TypeError String -- Expected type
                             String -- Actual type
                             String -- Expression
  
  instance Show TypeError where
    show (ParseError t l) = "Not a valid '" ++ t ++ "' literal: " ++ l
    show (TypeError t t' e) = "Couldn't match expected type '" ++ t
                              ++ "' with actual type '" ++ t' ++ "': " ++ e
  
  type TypeCheck t = forall m. (Monad m) => ExceptT TypeError m t
  
  
  parseerror :: String -> String -> TypeCheck a
  parseerror t l = throwError (ParseError t l)
  
  typeerror :: String -> String -> String -> TypeCheck a
  typeerror t t' e = throwError (TypeError t t' e)
  
  
  typeof :: Selection -> String
  typeof Name = "string"
  typeof Date = "date"
  typeof Size = "size"
  -- -------------------------------------------------------------------------------------
  
  
  -- Literals ----------------------------------------------------------------------------
  class Literal v where
    convert :: String -> TypeCheck (Expr v)
  
  
  instance Literal String where
    convert s = return (Value s)
  
  instance Literal Day where
    convert s = maybe (parseerror "date" s) (return . Value) $ readMaybe s
  
  instance Literal FileSize where
    convert s = maybe (parseerror "size" s) (return . Value) $ readMaybe s
  
  instance Literal Regex where
    convert s = withExceptT (ParseError "regex")
              $ Value <$> makeRegex s
  -- -------------------------------------------------------------------------------------
  
  -- Selection ---------------------------------------------------------------------------
  class (Ord t) => OrdSelection t where
    selection :: Selection -> TypeCheck (Expr t)
  
  
  instance OrdSelection String where
    selection Name = return (Sel SName)
    selection s = typeerror "string" (typeof s) (show s)
  
  instance OrdSelection Day where
    selection Date = return (Sel SDate)
    selection s = typeerror "date" (typeof s) (show s)
  
  instance OrdSelection FileSize where
    selection Size = return (Sel SSize)
    selection s = typeerror "size" (typeof s) (show s)
  -- -------------------------------------------------------------------------------------
  
  
  -- typecheck ---------------------------------------------------------------------------
  typecheck :: U.Expr -> TypeCheck (Expr Bool)
  
  typecheck (U.Sel s) = typeerror "bool" (typeof s) (show s)
  
  typecheck (U.Val v) = typeerror "bool" "string" v -- default type is string
  
  typecheck (U.Op (U.:!:) e) = (:!:) <$> typecheck e
  
  typecheck (U.BinOp op e e') =
    case op of (U.:&&:) -> (:&&:) <$> typecheck e <*> typecheck e'
               (U.:||:) -> (:||:) <$> typecheck e <*> typecheck e'
               (U.:>:)  -> ord (:>:)
               (U.:<:)  -> ord (:<:)
               (U.:>=:) -> ord (:>=:)
               (U.:<=:) -> ord (:<=:)
               (U.:==:) -> ord (:==:)
               (U.:/=:) -> ord (:/=:)
               (U.:=~:) -> match
    where
      ord :: (forall t. OrdOperator t) -> TypeCheck (Expr Bool)
      ord op' | Just s <- get_selection =
                  case s of Name -> (op' :: OrdOperator String  ) <$> atom e  (typeof s)
                                                                  <*> atom e' (typeof s)
                            Date -> (op' :: OrdOperator Day     ) <$> atom e  (typeof s)
                                                                  <*> atom e' (typeof s)
                            Size -> (op' :: OrdOperator FileSize) <$> atom e  (typeof s)
                                                                  <*> atom e' (typeof s)
              -- default to string:
              | otherwise = (op' :: OrdOperator String) <$> atom e  "string"
                                                        <*> atom e' "string"
      
      
      match :: TypeCheck (Expr Bool)
      match = case (e, e') of (U.Sel s, U.Val v)  -> case s of Name -> (Sel SName :=~:)
                                                               Date -> (Sel SDate :=~:)
                                                               Size -> (Sel SSize :=~:)
                                                    <$> convert v
                              
                              (U.Val v, U.Val v') -> (:=~:) <$> convert v <*> convert v'
                              
                              (_, U.Val _) -> typeerror "string" "bool"     (show e)
                              (_, U.Sel s) -> typeerror "regex"  (typeof s) (show e')
                              (_, _)       -> typeerror "regex"  "bool"     (show e')
      
      
      get_selection :: Maybe Selection
      get_selection = case (e, e') of (U.Sel s, _) -> Just s
                                      (_, U.Sel s) -> Just s
                                      _            -> Nothing
      
      atom :: (OrdSelection t, Literal t) => U.Expr -- Expression expected to be an atom.
                                          -> String -- The expected type.
                                          -> TypeCheck (Expr t)
      atom (U.Val v) _ = convert v
      atom (U.Sel s) _ = selection s
      atom e t = typeerror t "bool" (show e)
  
  
  type OrdOperator t = (Ord t) => Expr t -> Expr t -> Expr Bool
  -- -------------------------------------------------------------------------------------
