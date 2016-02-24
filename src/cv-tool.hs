module Main (main) where

import Control.Monad
import System.Console.GetOpt
import System.IO
import CVTool
import CVTool.Data


main = do
  args <- getArgs
  let (options, _, _) = getOpt Permute optionDescriptions args
  let 
