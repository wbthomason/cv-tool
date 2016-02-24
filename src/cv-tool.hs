module Main (main) where

import CVTool.Options
import CVTool.Reader
import CVTool.Writer
import Control.Monad
import Data.List
import System.Console.GetOpt
import System.Environment
import System.FilePath
import System.IO

main = do
  args <- getArgs
  let (options, _, _) = getOpt Permute toolOptionDescs args
  let options' = foldl (flip id) defaultOptions options
  let ToolOptions {
    optHelp = help,
    optInFile = inFile,
    optOutFile = outFile,
    optTemplate = template
  } = options'
  when help printHelp
  let reader = case takeExtension inFile of
                    ".yaml" ->  readYaml  
                    ".toml" ->  readToml
                    ".json" ->  readJson
  let writer = case takeExtension outFile of
                    ".pdf"  ->  writePdf
                    ".tex"  ->  writeTex
                    ".md"   ->  writeMarkdown
                    ".json" ->  writeJson
                    ".html" ->  writeHtml
  templateData <- readFile template
  inputData <- readFile inFile
  writeFile outFile $ writer templateData $ reader inputData
