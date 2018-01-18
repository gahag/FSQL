{- FSQL : FileInfo.hs -- File types and functions
 -
 - Copyright (C) 2015 gahag
 - All rights reserved.
 -
 - This software may be modified and distributed under the terms
 - of the BSD license. See the LICENSE file for details.
 -}

{-# LANGUAGE LambdaCase, TupleSections #-}

module FileInfo (
    Day, FileStatus, FileSize, FileInfo,
    name, date, size,
    getDirInfo
  ) where
  
  import Control.Monad  (foldM)
  import Control.Arrow  (first)
  
  -- Date
  import Data.Time                (Day, utctDay)
  import Data.Time.Clock.POSIX    (posixSecondsToUTCTime)
  import System.PosixCompat.Files (modificationTime)
  
  -- Size
  import System.Posix.Types       (FileOffset)
  import System.PosixCompat.Files (fileSize)
  
  -- File and directory contents
  import Data.List                ((\\))
  import System.Directory         (doesDirectoryExist, getDirectoryContents)
  import System.FilePath          ((</>))
  import System.PosixCompat.Files (FileStatus, getSymbolicLinkStatus)
  
  
  -- FileInfo : (filename, FileStatus)
  -- Must keep the filename since the FileStatus can't provide it.
  type FileInfo = (String, FileStatus)
  
  type FileSize = FileOffset
  
  
  name :: FileInfo -> String
  name = fst
  
  date :: FileInfo -> Day
  date = utctDay . posixSecondsToUTCTime . realToFrac . modificationTime . snd
  
  size :: FileInfo -> FileSize
  size = fileSize . snd
  
  
  -- getDirInfo --------------------------------------------------------------------------
  getDirInfo :: FilePath      -- The directory to list.
             -> Bool          -- Wether to list recursively.
             -> IO [FileInfo]
  
  getDirInfo path False = (\\ [".", ".."]) <$> getDirectoryContents path
                      >>= mapM (\ fname -> (fname,) <$> getSymlinkStatus (path </> fname))
    where
      getSymlinkStatus = getSymbolicLinkStatus
  
  getDirInfo path True = getDirInfo' path ""
    where
      getDirInfo' :: FilePath -> FilePath -> IO [FileInfo]
      getDirInfo' root path =
        getDirInfo (root </> path) False -- List the files in the path, then recurse for
          >>= foldM (                    -- the directories.
                \ fs f -> let f' = first (path </>) f -- Prefix the path to every filename
                              fs' = fs ++ [f']        -- and add it to the list.
                          in (fs' ++) <$> do isdir <- dirExists (root </> name f')
                                             if isdir then getDirInfo' root (name f')
                                                      else return [] -- file already added.
              ) []
      
      dirExists = doesDirectoryExist
  -- -------------------------------------------------------------------------------------
