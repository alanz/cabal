{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.ImplicitConstraints
-- Copyright   :  Alan Zimmerman, 2016
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Manage inserting implicit constraints into a cabal file
-- See https://github.com/haskell/cabal/issues/3729

module Distribution.ImplicitConstraints (
  insertImplicitConstraints
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import Data.Functor.Identity
import qualified Data.Map as Map
import Distribution.Compiler
import Distribution.Package
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.Parse
import Distribution.PackageDescription.PrettyPrint
import Distribution.System
import Distribution.Types.Benchmark
import Distribution.Types.BuildInfo
import Distribution.Types.Executable
import Distribution.Types.GenericPackageDescription
import Distribution.Types.Library
import Distribution.Types.TestSuite
import Distribution.Verbosity
import Distribution.Version

-- ---------------------------------------------------------------------
-- |Insert implicit constraints into a Cabal file, for use by the resolver at
-- the client side. This is intended to be used at the server side.
insertImplicitConstraints :: [Dependency] ->  GenericPackageDescription -> GenericPackageDescription
insertImplicitConstraints implicits pkg = transformAllBuildInfos addDepsToBI id pkg
  where
    addDepsToBI :: BuildInfo -> BuildInfo
    addDepsToBI bi = bi { targetBuildDepends = addDeps (targetBuildDepends bi)}

    addDeps :: [Dependency] -> [Dependency]
    addDeps ds = case matchingDeps ds of
      [] -> ds
      ibs -> ds ++ [headerCommentDep] ++ ibs

    headerCommentDep = Dependency (PackageName "-- IMPLICIT CONSTRAINTS ADDED BY HACKAGE") anyVersion

    matchingDeps :: [Dependency] -> [Dependency]
    matchingDeps deps = catMaybes $ map (\(Dependency pn _) -> Map.lookup pn implicitMap) deps

    implicitMap = Map.fromList $ map (\p@(Dependency pn _) -> (pn,p)) implicits

-- ---------------------------------------------------------------------

implicitBounds :: [Dependency]
implicitBounds = [Dependency (PackageName "directory") (earlierVersion (Version [0,2,3] []))]

-- ---------------------------------------------------------------------

tt = do
  -- pkg <- readPackageDescription normal "/home/alanz/tmp/cabal-index-unpacked/acme-box/0.0.0.0/acme-box.cabal"
  pkg <- readPackageDescription normal "/home/alanz/tmp/cabal-index-unpacked/alpha/1.0.9/alpha.cabal"

  let pkg' = insertImplicitConstraints implicitBounds pkg
  putStrLn $ showGenericPackageDescription pkg'
  return ()
