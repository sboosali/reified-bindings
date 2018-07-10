{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

{-|

running manually:

@
stack build && stack exec -- reified-bindings --help
@

usage:

@
    {-# OPTIONS_GHC -F -pgmF reified-bindings -optF ReifiedBindings.Grammar #-}
@

-}
module ReifiedBindings
 ( module ReifiedBindings
 , module ReifiedBindings.Types
 ) where

import ReifiedBindings.Extra
import ReifiedBindings.Types

import Options.Generic

import System.IO

--------------------------------------------------------------------------------

main :: IO ()
main = getRecord description >>= preprocessReifiedBindings
 where
 description = "reified-bindings"

{-|

-}
preprocessReifiedBindings :: PreProcessorArguments -> IO ()
preprocessReifiedBindings (PreProcessorArguments (unHelpful -> sourcePath) (unHelpful -> inputPath) (unHelpful -> outputPath) (unHelpful -> moduleName)) = do

  -- putStrLn ""
  -- putStrLn sourcePath
  -- putStrLn inputPath
  -- putStrLn outputPath
  -- putStrLn moduleName

  inHandle  <- openFile inputPath  ReadMode
  outHandle <- openFile outputPath WriteMode

  input <- hGetContents inHandle
  hPutStr outHandle input

  let _debug s = hPutStrLn outHandle ("  putStrLn " ++ show s)
  traverse_ _debug [sourcePath,inputPath,outputPath,moduleName]

  hClose inHandle
  hClose outHandle
  return ()

--------------------------------------------------------------------------------
