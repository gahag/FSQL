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
  import qualified Prelude as P (Either(..))

  import Control.Applicative ((<$>), (<*>))
  import Control.Arrow       ((&&&))
  import Data.Function       (on)
  import Data.List           ((\\), sort, deleteFirstsBy, intersectBy, unionBy)

  import System.Directory (doesDirectoryExist, getDirectoryContents)
  import System.FilePath  ((</>))
  
  import Control.Monad.Trans.Either (EitherT(..))

  import FileInfo (FileInfo, getFileStatus, name, date, size)
  
  
  
  data Query = Query Selection Source (Maybe Predicate)
  
  data Selection = Name
                 | Date
                 | Size
  
  data Source = Single FilePath
              | Join Join (FilePath, FilePath) Selection
  
  data Join = Inner
            | Left 
            | Right
            | Outer
            | Full
  
  type Predicate = FileInfo -> Bool
  
  
  instance Show Selection where
    show = \case Name -> "name"
                 Date -> "date"
                 Size -> "size"
  
  
  
  fetch_query :: Query -> EitherT String IO [String]
  fetch_query (Query sel source pred) = (<$> fetch_source source) $
    sort . case pred of
            Nothing -> map (selector sel)
            Just p  -> flip foldr [] $ \ x -> if p x then (selector sel x :)
                                                     else id
  
  
  -- fetch_source --------------------------------------------------------------
  fetch_source :: Source -> EitherT String IO [FileInfo]

  fetch_source (Single s) = EitherT $ doesDirectoryExist s >>=
    \case False -> return . P.Left
                    $ "Error: Directory \"" ++ s ++ "\" not found!"
          
          True  -> (\\ [".", ".."])
                    <$> getDirectoryContents s
                    >>= fmap P.Right . mapM (getFileInfo s)

  fetch_source (Join j (s, s') sel) = joiner j (eq_on_sel sel)
                                        <$> fetch_source (Single s)
                                        <*> fetch_source (Single s')
  -- ---------------------------------------------------------------------------

  
  getFileInfo :: FilePath -> FilePath -> IO FileInfo
  getFileInfo path name = (name,) <$> getFileStatus (path </> name)
  
  
  joiner :: Join -> (a -> a -> Bool) -> ([a] -> [a] -> [a])
  joiner = \case Inner -> intersectBy
                 Left  -> deleteFirstsBy
                 Right -> flip . deleteFirstsBy
                 Outer -> \ f x x' -> joiner Left f x x' ++ joiner Right f x x'
                 Full  -> unionBy
  
  selector :: Selection -> (FileInfo -> String)
  selector = \case Name -> name
                   Date -> show . date
                   Size -> show . size
  
  eq_on_sel :: Selection -> (FileInfo -> FileInfo -> Bool)
  eq_on_sel = \case Name -> (==) `on` name
                    Date -> (==) `on` date
                    Size -> (==) `on` size
