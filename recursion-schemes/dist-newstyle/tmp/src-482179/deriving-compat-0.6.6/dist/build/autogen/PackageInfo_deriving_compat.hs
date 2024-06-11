{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_deriving_compat (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "deriving_compat"
version :: Version
version = Version [0,6,6] []

synopsis :: String
synopsis = "Backports of GHC deriving extensions"
copyright :: String
copyright = "(C) 2015-2017 Ryan Scott"
homepage :: String
homepage = "https://github.com/haskell-compat/deriving-compat"
