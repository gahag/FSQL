{- FSQL : Query.hs -- Query data types, instances and functions
 -
 - Copyright (C) 2015 gahag
 - All rights reserved.
 -
 - This software may be modified and distributed under the terms
 - of the BSD license. See the LICENSE file for details.
 -}

{-# LANGUAGE LambdaCase, TupleSections #-}

module Query (
    Query(..), Selection(..), Source(..), JoinType(..), Predicate,
    fetch_query
  ) where
  
  import Prelude hiding (Either(..))
  
  import Control.Monad.Except (ExceptT, throwError)
  import Control.Monad.Trans  (lift)
  import Data.Function        (on)
  import Data.List            (sort, deleteFirstsBy, intersectBy, unionBy)
  
  import System.Directory (doesDirectoryExist)
  import System.FilePath  ((</>))
  
  import FileInfo (FileInfo, getFileStatus, name, date, size
                  , getDirContents, getDirContentsRec)
  
  
  
  data Query = Query [Selection] Source (Maybe Predicate)
  
  -- Selection : Represents the information to be extracted from a file.
  data Selection = Name
                 | Date
                 | Size
  
  -- Source : Source directory for the query.
  data Source = Source { path    :: FilePath
                       , recurse :: Bool     }
              | Join JoinType (Source, Source) Selection -- Join two sources on selection
  
  data JoinType = Inner
                | Left 
                | Right
                | Outer
                | Full
  
  -- Predicate : Takes the FileInfo and returns if it should be filtered.
  type Predicate = FileInfo -> Bool
  
  
  instance Show Selection where
    show = \case Name -> "name"
                 Date -> "date"
                 Size -> "size"
  
  
  
  fetch_query :: Query -> ExceptT String IO [[String]]
  fetch_query (Query sel source predicate) = (<$> fetch_source source) $
    sort . case predicate of
            Nothing -> map (selectors sel)
            Just p  -> flip foldr [] $ \ x -> if p x then (selectors sel x :)
                                                     else id
  
  -- fetch_source --------------------------------------------------------------
  fetch_source :: Source -> ExceptT String IO [FileInfo]
  
  fetch_source (Join joinType (s, s') sel) = joiner joinType (eq_selector sel)
                                          <$> fetch_source s
                                          <*> fetch_source s'
  
  fetch_source src = lift (doesDirectoryExist $ path src) >>=
    \case False -> throwError ("Error: Directory \"" ++ path src ++ "\" not found!")
          True  -> lift (fetch_files (path src) >>= mapM fetch_fileinfo)
    where
      fetch_files | recurse src = getDirContentsRec
                  | otherwise   = getDirContents
      
      fetch_fileinfo fName = (fName,) <$> getFileStatus (path src </> fName)
  -- ---------------------------------------------------------------------------
  
  selectors :: [Selection] -> (FileInfo -> [String])
  -- Returns a function that extracts the selection info from the FileInfo.
  -- The order of the selections is maintained.
  selectors sels = \ fi -> map (`selector` fi) sels
    where
      selector = \case Name -> name
                       Date -> show . date
                       Size -> show . size
  
  eq_selector :: Selection -> (FileInfo -> FileInfo -> Bool)
  eq_selector = \case Name -> (==) `on` name
                      Date -> (==) `on` date
                      Size -> (==) `on` size
  
  
  joiner :: JoinType -> (a -> a -> Bool) -> ([a] -> [a] -> [a])
  -- Returns a function to make the join based on a equality comparer.
  joiner = \case Inner -> intersectBy
                 Left  -> deleteFirstsBy
                 Right -> flip . deleteFirstsBy
                 Outer -> \ f x x' -> joiner Left f x x' ++ joiner Right f x x'
                 Full  -> unionBy
