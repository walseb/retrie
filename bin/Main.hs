-- Copyright (c) 2025 Andrew Farmer
-- Copyright (c) 2020-2024 Facebook, Inc. and its affiliates.
--
-- This source code is licensed under the MIT license found in the
-- LICENSE file in the root directory of this source tree.
--
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Control.Monad
import Fixity
import qualified GHC.Paths as GHC.Paths
import Retrie
import Retrie.Debug
import Retrie.Options
import Retrie.Run

main :: IO ()
main = do
  let libdir = GHC.Paths.libdir
  opts@Options{..} <- parseOptions libdir defaultFixityEnv
  doRoundtrips libdir fixityEnv targetDir roundtrips
  unless (null rewrites) $ do
    when (verbosity > Silent) $ do
      putStrLn "Adding:"
      mapM_ (putStrLn . ppRewrite) rewrites
    -- Search only needs the matched spans. Keeping each match unchanged avoids exact-printing an arbitrary placeholder RHS into the module; that can corrupt layout-sensitive expressions before writeSearch reports them.
    let rewrites' = case executionMode of
          ExecSearch -> identityRewrite <$> rewrites
          _ -> rewrites
    execute libdir opts $ apply rewrites'
  where
    identityRewrite q@Query{qPattern = pat, qResult = (t, tr)} =
      q {qResult = (t {tTemplate = pat}, tr)}
