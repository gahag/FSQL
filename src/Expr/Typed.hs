{- FSQL : Expr/Typed.hs -- Typed expression data types and functions
 -
 - Copyright (C) 2015 gahag
 - All rights reserved.
 -
 - This software may be modified and distributed under the terms
 - of the BSD license. See the LICENSE file for details.
 -}

{-# LANGUAGE GADTs, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE DataKinds, KindSignatures #-}

module Expr.Typed (
    Sel(..), Expr(..), Typed(..),
    compile
  ) where
  
  import FileInfo (FileInfo, Day, FileSize, name, date, size)
  import Query    (Selection(..))
  import Regex    (Regex, match)
  
  
  data Sel (s :: Selection) where -- Singleton type to keep track of the used Selection.
    SName :: Sel 'Name
    SDate :: Sel 'Date
    SSize :: Sel 'Size
  
  class Typed t t' where
    coerce :: t -> (FileInfo -> t')
  
  
  instance Typed (Sel 'Name) String where -- ord, regex
    coerce SName = name
  
  
  instance Typed (Sel 'Date) Day where -- ord
    coerce SDate = date
  
  instance Typed (Sel 'Date) String where -- regex
    coerce SDate = show . date
  
  
  instance Typed (Sel 'Size) FileSize where -- ord
    coerce SSize = size
  
  instance Typed (Sel 'Size) String where -- regex
    coerce SSize = show . size
  
  
  data Expr t where -- Expression of type t
    Sel   :: (Typed (Sel s) v) => Sel s -> Expr v
    Value ::                      v     -> Expr v
    --                   lhs         -> rhs        -> expr t
    (:!:)  ::                           Expr Bool  -> Expr Bool
    (:&&:) ::            Expr Bool   -> Expr Bool  -> Expr Bool
    (:||:) ::            Expr Bool   -> Expr Bool  -> Expr Bool
    (:>:)  :: (Ord t) => Expr t      -> Expr t     -> Expr Bool
    (:<:)  :: (Ord t) => Expr t      -> Expr t     -> Expr Bool
    (:>=:) :: (Ord t) => Expr t      -> Expr t     -> Expr Bool
    (:<=:) :: (Ord t) => Expr t      -> Expr t     -> Expr Bool
    (:==:) :: (Eq t)  => Expr t      -> Expr t     -> Expr Bool
    (:/=:) :: (Eq t)  => Expr t      -> Expr t     -> Expr Bool
    (:=~:) ::            Expr String -> Expr Regex -> Expr Bool
  
  
  
  compile :: Expr t -> (FileInfo -> t)
  compile (Sel s)     = coerce s
  compile (Value v)   = const v
  compile ((:!:) e)   = not . compile e
  compile (e :&&: e') = (&&) <$> compile e <*> compile e'
  compile (e :||: e') = (||) <$> compile e <*> compile e'
  compile (e :>: e')  = (>)  <$> compile e <*> compile e'
  compile (e :<: e')  = (<)  <$> compile e <*> compile e'
  compile (e :>=: e') = (>=) <$> compile e <*> compile e'
  compile (e :<=: e') = (<=) <$> compile e <*> compile e'
  compile (e :==: e') = (==) <$> compile e <*> compile e'
  compile (e :/=: e') = (/=) <$> compile e <*> compile e'
  compile (e :=~: e') = flip match <$> compile e <*> compile e'
