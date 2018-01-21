{- FSQL : Version.hs -- Version info
 -
 - Copyright (C) 2015 gahag
 - All rights reserved.
 -
 - This software may be modified and distributed under the terms
 - of the BSD license. See the LICENSE file for details.
 -}

module Version (
    aboutMsg, version
  ) where
  
  import Data.Version (showVersion)
  
  import Paths_FSQL (version)
  
  
  aboutMsg :: String
  aboutMsg = "FSQL v" ++ showVersion version ++ " - hosted at https://github.com/gahag/FSQL"
