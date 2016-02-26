module Main (main) where

import CVTool.Options
import CVTool.Readers
import CVTool.Writers
import Control.Monad
import Data.ByteString (readFile)
import Data.List
import Prelude hiding (readFile)
import System.Console.GetOpt
import System.Environment
import System.FilePath
import System.IO hiding (readFile)

main = do
  args <- getArgs
  let (options, _, _) = getOpt Permute toolOptionDescs args
  let options' = foldl (flip id) defaultOptions options
  let ToolOptions {
    optHelp       = help,
    optInFile     = inFile,
    optOutFile    = outFile,
    optTemplate   = template,
    optPdfCreator = pdfCreator
  } = options'
  when help printHelp
  let reader = case takeExtension inFile of
                    ".yaml" ->  readYaml  
                    ".toml" ->  readToml
                    ".json" ->  readJson
  let writer = case takeExtension outFile of
                    ".pdf"  ->  writePDF
                    ".tex"  ->  writeLaTeX
                    ".md"   ->  writeMarkdown
                    ".json" ->  writeJSON
                    ".html" ->  writeHtml
  templateData <- readFile template
  inputData <- readFile inFile
  writeFile outFile $ writer templateData $ reader inputData
