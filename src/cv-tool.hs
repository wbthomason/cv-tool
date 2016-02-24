module Main (main) where

import CVTool.Options
import CVTool.Reader
import CVTool.Writer
import Control.Monad
import Data.List
import System.Console.GetOpt
import System.Environment
import System.IO

updateOptions :: ToolOptions -> (ToolOptions -> ToolOptions) -> ToolOptions
updateOptions base option = option base

main = do
  args <- getArgs
  let (options, _, _) = getOpt Permute toolOptionDescs args
  let options' = foldl (flip id) defaultOptions options
  let ToolOptions {
    optHelp = help,
    optInFile = inFile,
    optOutFile = outFile
  } = options'
  when help printHelp
