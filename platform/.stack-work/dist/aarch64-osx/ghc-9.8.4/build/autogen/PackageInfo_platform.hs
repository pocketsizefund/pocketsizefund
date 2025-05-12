{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_platform (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "platform"
version :: Version
version = Version [2025,4,14,1] []

synopsis :: String
synopsis = ""
copyright :: String
copyright = "2025 pocketsizefund"
homepage :: String
homepage = "https://github.com/pocketsizefund/platform#readme"
