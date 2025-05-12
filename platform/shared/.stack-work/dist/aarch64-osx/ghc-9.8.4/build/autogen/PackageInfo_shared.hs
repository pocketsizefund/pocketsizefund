{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_shared (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "shared"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Shared resources for the application"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
