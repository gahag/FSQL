{- FSQL : FileInfo.hs -- File types and functions
 -
 - Copyright (C) 2015 gahag
 - All rights reserved.
 -
 - This software may be modified and distributed under the terms
 - of the BSD license. See the LICENSE file for details.
 -}

{-# LANGUAGE LambdaCase #-}

module FileInfo (
    Day, FileStatus, FileSize, FileInfo,
    getSymbolicLinkStatus,
    name, date, size,
    getDirContents, getDirContentsRec
  ) where
  
  import System.PosixCompat.Files (FileStatus, getSymbolicLinkStatus)
  
  -- Date
  import Data.Time                (Day, utctDay)
  import Data.Time.Clock.POSIX    (posixSecondsToUTCTime)
  import System.PosixCompat.Files (modificationTime)
  
  -- Size
  import System.Posix.Types       (FileOffset)
  import System.PosixCompat.Files (fileSize)
  
  -- Directory Contents
  import Data.List        ((\\))
  import System.Directory (doesDirectoryExist, getDirectoryContents)
  import System.FilePath  ((</>))
  
  
  
  -- FileInfo : (filename, FileStatus)
  -- Must keep the filename since the FileStatus can't provide it.
  type FileInfo = (String, FileStatus)
  
  type FileSize = FileOffset
  
  
  
  name :: FileInfo -> String
  name = fst
  
  date :: FileInfo -> Day
  date = utctDay . posixSecondsToUTCTime . realToFrac . modificationTime . snd
  
  size :: FileInfo -> FileOffset
  size = fileSize . snd
  
  
  getDirContents :: FilePath -> IO [FilePath]
  getDirContents dir = (\\ [".", ".."]) -- remove '.' and '..'
                        <$> getDirectoryContents dir
  
  getDirContentsRec :: FilePath -> IO [FilePath]
  getDirContentsRec dir =
    do contents <- getDirContents dir
       paths <- (`mapM` contents) $
                  \ name -> doesDirectoryExist (dir </> name)
                        >>= \case True  -> getDirContentsRec' dir name
                                  False -> return [name]
       return (concat paths)
    where
      getDirContentsRec' root dir =
        do contents <- getDirContents (root </> dir)
           paths <- (`mapM` contents) $
                      \ name -> let path = dir </> name in
                                doesDirectoryExist (root </> path)
                            >>= \case True  -> getDirContentsRec' root path
                                      False -> return [path]
           return $
            if null contents then [dir]
                             else concat paths
