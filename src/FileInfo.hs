{- FSQL : FileInfo.hs -- File types and functions
 -
 - Copyright (C) 2015 gahag
 - All rights reserved.
 -
 - This software may be modified and distributed under the terms
 - of the BSD license. See the LICENSE file for details.
 -}

{-# LANGUAGE TupleSections #-}

module FileInfo (
    Day, FileStatus, FileSize, FileInfo,
    name, date, size,
    getDirInfo
  ) where
  
  import Control.Arrow        (first)
  import Control.Monad        (foldM)
  import Control.Monad.Except (ExceptT(..), lift)
  
  -- Date
  import Data.Time                (Day, utctDay)
  import Data.Time.Clock.POSIX    (posixSecondsToUTCTime)
  import System.PosixCompat.Files (modificationTime)
  
  -- Size
  import System.Posix.Types       (FileOffset)
  import System.PosixCompat.Files (fileSize)
  
  -- File and directory contents
  import Data.List                ((\\))
  import System.Directory         (getDirectoryContents, getHomeDirectory)
  import System.FilePath          ((</>), normalise)
  import System.IO.Error          (IOError, tryIOError)
  import System.PosixCompat.Files (FileStatus, getSymbolicLinkStatus, isDirectory)
  
  
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
  
  isDir :: FileInfo -> Bool
  isDir = isDirectory . snd
  
  
  -- getDirInfo --------------------------------------------------------------------------
  -- The IOError might be:
  -- HardwareFault        : A physical I/O error has occurred.
  -- isDoesNotExistError  : There is no path referring to the working directory.
  -- isPermissionError    : Insufficient privileges to perform the operation.
  -- ResourceExhausted    : Insufficient resources are available to perform the operation.
  -- UnsupportedOperation : The operating system has no notion of current working directory.
  -- InappropriateType    : The path refers to an object that is not a directory.
  
  getDirInfo, getDirInfo' :: FilePath        -- The directory to list.
                          -> Bool            -- Wether to list recursively.
                          -> ExceptT IOError IO [FileInfo]
  
  
  -- Handle the tilde as a synonym to the home directory:
  getDirInfo path@('~':_) rec = do path' <- tildeExpand path
                                   getDirInfo' path' rec
  
  getDirInfo path rec = getDirInfo' path rec
  
  
  getDirInfo' path False = (\\ [".", ".."]) <$> ioExcept (getDirectoryContents path)
                       >>= mapM getFileInfo
    where
      getFileInfo :: String -> ExceptT IOError IO FileInfo
      getFileInfo fname = (fname,) <$> ioExcept (getSymbolicLinkStatus (path </> fname))
  
  getDirInfo' path True = recDirInfo path ""
    where
      recDirInfo :: FilePath -> FilePath -> ExceptT IOError IO [FileInfo]
      recDirInfo root path =
        getDirInfo (root </> path) False -- List the files in the path, then recurse for
          >>= foldM (                    -- the directories.
                \ fs f -> let f' = first (path </>) f -- Prefix the path to every filename
                              fs' = fs ++ [f']        -- and add it to the list.
                          in
                            if isDir f' then (fs' ++) <$> recDirInfo root (name f')
                                        else return fs'
              ) []
  
  
  tildeExpand :: FilePath -> ExceptT IOError IO FilePath
  tildeExpand "~"            = ioExcept getHomeDirectory
  tildeExpand ('~':'/':path) = let path' = tail      -- Remove the leading "/"
                                         . normalise -- Remove excess separators
                                         $ '/':path  -- Make sure there is at least "/"
                                in (</> path') <$> ioExcept getHomeDirectory
  tildeExpand path = return path
  
  
  ioExcept :: IO a -> ExceptT IOError IO a
  ioExcept = ExceptT . tryIOError
  -- -------------------------------------------------------------------------------------
