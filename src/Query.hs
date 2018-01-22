{- FSQL : Query.hs -- Query data types, instances and functions
 -
 - Copyright (C) 2015 gahag
 - All rights reserved.
 -
 - This software may be modified and distributed under the terms
 - of the BSD license. See the LICENSE file for details.
 -}

{-# LANGUAGE LambdaCase #-}

module Query (
    Query(..), Selection(..), Source(..), JoinType(..), Predicate,
    FetchError(..),
    fetch_query
  ) where
  
  import Prelude hiding (Either(..))
  
  import Control.Monad.Except (ExceptT, withExceptT)
  import Data.Function        (on)
  import Data.List            (deleteFirstsBy, intersectBy, unionBy)
  
  import System.IO.Error  (ioeGetFileName, ioeGetErrorType
                          , isDoesNotExistError, isPermissionError)
  
  import GHC.IO.Exception (IOErrorType(InappropriateType)) -- GHC specific error thrown
                                                           -- by getDirectoryContents.
  
  import FileInfo (FileInfo, name, date, size, getDirInfo)
  
  
  
  data Query = Query [Selection] Source (Maybe Predicate)
  
  -- Selection : Represents the information to be extracted from a file.
  data Selection = Name
                 | Date
                 | Size
  
  instance Show Selection where
    show = \case Name -> "name"
                 Date -> "date"
                 Size -> "size"
  
  -- Source : Source directory for the query.
  data Source = Source Bool      -- Wether to recursively fetch.
                       FilePath  -- Single path
              | Join Bool                 -- Wether to recursively fetch.
                     JoinType             -- Inner, outer, etc.
                     (FilePath, FilePath) -- Join two paths
                     Selection            -- Join by name, date, etc.
  
  data JoinType = Inner
                | Left 
                | Right
                | Outer
                | Full
  
  -- Predicate : Takes the FileInfo and returns if it should be filtered.
  type Predicate = FileInfo -> Bool
  
  
  data FetchError = NotADirectory (Maybe FilePath)
                  | NotFound (Maybe FilePath)
                  | Permission (Maybe FilePath)
                  | IOErr IOError
  
  instance Show FetchError where
    show (NotADirectory p) = "Not a directory" ++ maybe "." (": " ++) p
    show (NotFound p)      = "No such file or directory" ++ maybe "." (": " ++) p
    show (Permission p)    = "Permission denied" ++ maybe "." (": " ++) p
    show (IOErr e)         = show e
  
  
  
  -- fetch_query -------------------------------------------------------------------------
  fetch_query :: Query -> ExceptT FetchError IO [[String]]
  fetch_query (Query sel src pred) = map (selectors sel)
                                   . case pred of Nothing -> id
                                                  Just p  -> filter p
                                  <$> fetch_source src
    where
      selectors :: [Selection] -> (FileInfo -> [String])
      -- Returns a function that extracts the selection info from the FileInfo.
      -- The order of the selections is maintained.
      selectors sels = \ fi -> map (`selector` fi) sels
        where
          selector = \case Name -> name
                           Date -> show . date
                           Size -> show . size
  -- -------------------------------------------------------------------------------------
  
  -- fetch_source ------------------------------------------------------------------------
  fetch_path :: FilePath -> Bool -> ExceptT FetchError IO [FileInfo]
  fetch_path path rec = withExceptT convert (getDirInfo path rec)
    where
      convert :: IOError -> FetchError
      convert e | isInappropriateType e = NotADirectory (ioeGetFileName e)
                | isDoesNotExistError e = NotFound (ioeGetFileName e)
                | isPermissionError e   = Permission (ioeGetFileName e)
                | otherwise             = IOErr e
      
      isInappropriateType e = ioeGetErrorType e == InappropriateType
  
  
  fetch_source :: Source -> ExceptT FetchError IO [FileInfo]
  fetch_source (Source rec path) = fetch_path path rec
  fetch_source (Join rec joinType (p, p') s) = joiner joinType (eq_selector s)
                                            <$> fetch_path p  rec
                                            <*> fetch_path p' rec
    where
      joiner :: JoinType -> (a -> a -> Bool) -> ([a] -> [a] -> [a])
      -- Returns a function to make the join based on a equality comparer.
      joiner = \case Inner -> intersectBy
                     Left  -> deleteFirstsBy
                     Right -> flip . deleteFirstsBy
                     Outer -> \ f x x' -> joiner Left f x x' ++ joiner Right f x x'
                     Full  -> unionBy
      
      eq_selector :: Selection -> (FileInfo -> FileInfo -> Bool)
      eq_selector = \case Name -> (==) `on` name
                          Date -> (==) `on` date
                          Size -> (==) `on` size
  -- -------------------------------------------------------------------------------------
