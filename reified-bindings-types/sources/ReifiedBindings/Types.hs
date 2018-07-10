{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveAnyClass #-}
{-# LANGUAGE DataKinds, TypeOperators     #-}

{-|

-}
module ReifiedBindings.Types where
import ReifiedBindings.Extra

import Options.Generic

--------------------------------------------------------------------------------

{-|

<https://downloads.haskell.org/~ghc/7.10.3/docs/html/users_guide/options-phases.html#pre-processor>

<https://hackage.haskell.org/package/optparse-generic>

e.g.

@
PreProcessorArguments
 "sources/ReifiedBindings/Example.hs"
 "sources/ReifiedBindings/Example.hs"
 "/var/folders/q7/yk68v9m53sl2zcsj_qph2hmm0000gn/T/ghc47996_0/ghc_1.hspp"
 "ReifiedBindings.Identity"
@

-}
data PreProcessorArguments = PreProcessorArguments
 --TODO White space is stripped, tabs don't work https://github.com/Gabriel439/Haskell-Optparse-Generic-Library/issues
 (FilePath <?> "{sourcePath :: FilePath} (via ghc)")
 (FilePath <?> "{inputPath  :: FilePath} (via ghc)")
 (FilePath <?> "{outputPath :: FilePath} (via ghc)")
 (String   <?> "{moduleName :: String}   e.g. ReifiedBindings.Identity")
  deriving (Show,Generic,ParseRecord)
