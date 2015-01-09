{- FSQL : FileInfo.hs -- File types and functions
 -
 - Copyright (C) 2015 gahag
 - All rights reserved.
 -
 - This software may be modified and distributed under the terms
 - of the BSD license. See the LICENSE file for details.
 -}

module FileInfo (
    Day, FileStatus, FileOffset, FileInfo,
    getFileStatus,
    name, date, size
  ) where
  
  import System.PosixCompat.Files (FileStatus, getFileStatus)
  
  -- Date
  import Data.Time                (Day, utctDay)
  import Data.Time.Clock.POSIX    (posixSecondsToUTCTime)
  import System.PosixCompat.Files (modificationTime)
  
  -- Size
  import System.Posix.Types       (FileOffset)
  import System.PosixCompat.Files (fileSize)
  
  
  
  type FileInfo = (String, FileStatus)
  
  
  
  name :: FileInfo -> String
  name = fst
  
  date :: FileInfo -> Day
  date = utctDay . posixSecondsToUTCTime . realToFrac . modificationTime . snd
  
  size :: FileInfo -> FileOffset
  size = fileSize . snd
