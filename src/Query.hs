{- FSQL : Query.hs -- Query data types, instances and functions
 -
 - Copyright (C) 2015 gahag
 - All rights reserved.
 -
 - This software may be modified and distributed under the terms
 - of the BSD license. See the LICENSE file for details.
 -}

{-# LANGUAGE LambdaCase, TupleSections #-}

module Query where
  
  import Prelude hiding (Either(..))
  
  import Control.Applicative        ((<$>), (<*>))
  import Control.Monad.IO.Class     (liftIO)
  import Control.Monad.Trans.Except (ExceptT, throwE)
  import Data.Function              (on)
  import Data.List                  ((\\), intercalate, sort
                                    , deleteFirstsBy, intersectBy, unionBy)
  
  import System.Directory (doesDirectoryExist, getDirectoryContents)
  import System.FilePath  ((</>))
  
  import FileInfo (FileInfo, getFileStatus, name, date, size)
  
  
  
  data Query = Query [Selection] Source (Maybe Predicate)
  
  -- Selection : Represents the information to be extracted from a file.
  data Selection = Name
                 | Date
                 | Size
  
  -- Source : Source directory for the query.
  data Source = Single FilePath
              | Join Join (FilePath, FilePath) Selection
  
  data Join = Inner
            | Left 
            | Right
            | Outer
            | Full
  
  --   Predicate : Takes the FileInfo and returns if it should be filtered.
  type Predicate = FileInfo -> Bool
  
  
  instance Show Selection where
    show = \case Name -> "name"
                 Date -> "date"
                 Size -> "size"
  
  
  
  fetch_query :: Query -> ExceptT String IO [String]
  fetch_query (Query sel source pred) = (<$> fetch_source source) $
    sort . case pred of
            Nothing -> map (selectors sel)
            Just p  -> flip foldr [] $ \ x -> if p x then (selectors sel x :)
                                                     else id
  
  
  -- fetch_source --------------------------------------------------------------
  fetch_source :: Source -> ExceptT String IO [FileInfo]

  fetch_source (Single s) = liftIO (doesDirectoryExist s) >>=
    \case False -> throwE ("Error: Directory \"" ++ s ++ "\" not found!")
          True  -> liftIO $ (\\ [".", ".."]) -- remove '.' and '..'
                              <$> getDirectoryContents s
                              >>= mapM (\ n -> (n,) <$> getFileStatus (s </> n))
  
  fetch_source (Join j (s, s') sel) = joiner j (eq_on_sel sel)
                                        <$> fetch_source (Single s)
                                        <*> fetch_source (Single s')
  -- ---------------------------------------------------------------------------
  
  
  joiner :: Join -> (a -> a -> Bool) -> ([a] -> [a] -> [a])
  -- Returns a function to make the join based on a equality comparer.
  joiner = \case Inner -> intersectBy
                 Left  -> deleteFirstsBy
                 Right -> flip . deleteFirstsBy
                 Outer -> \ f x x' -> joiner Left f x x' ++ joiner Right f x x'
                 Full  -> unionBy
  
  selectors :: [Selection] -> (FileInfo -> String)
  -- Returns a function that extracts the selection info from the FileInfo.
  -- The selections are separated by a tab, and their order is maintained.
  selectors sels fi = intercalate "\t" $ map (flip selector fi) sels
    where
      selector = \case Name -> name
                       Date -> show . date
                       Size -> show . size
  
  eq_on_sel :: Selection -> (FileInfo -> FileInfo -> Bool)
  eq_on_sel = \case Name -> (==) `on` name
                    Date -> (==) `on` date
                    Size -> (==) `on` size
