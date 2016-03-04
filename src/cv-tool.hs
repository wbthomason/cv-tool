module Main (main) where

import CVTool.Options
import CVTool.Readers
import CVTool.Writers
import Control.Monad
import qualified Data.ByteString as B (readFile, writeFile)
import Data.ByteString.Lazy (fromStrict)
import Data.List as List
import System.Console.GetOpt
import System.Environment
import System.FilePath
import System.IO
import System.Exit
import Data.Text.Encoding (decodeUtf8)
import Debug.Trace

main = do
  args <- getArgs
  let (options, _, _) = getOpt Permute toolOptionDescs args
  let options' = List.foldl (flip id) defaultOptions options
  let ToolOptions {
    optHelp       = help,
    optInFile     = inFile,
    optOutFile    = outFile,
    optTemplate   = template,
    optPdfCreator = pdfCreator
  } = options'
  when help printHelp
  inputData <- B.readFile inFile
  let reader = case takeExtension inFile of
                    ".yaml" ->  readYaml  
                    ".toml" ->  readToml . decodeUtf8
                    ".json" ->  readJson . fromStrict
  let textWriter = writeFile outFile
  let binWriter = B.writeFile outFile
  let (template', writer) = case takeExtension outFile of
                    ".tex"  ->  (template ++ ".tex", \t p -> textWriter $ writeLaTeX t p)
                    ".md"   ->  (template ++ ".md", \t p -> textWriter $ writeMarkdown t p)
                    ".json" ->  (template ++ ".json", \t p -> textWriter $ writeJSON t p)
                    ".html" ->  (template ++ ".html", \t p -> textWriter $ writeHtml t p)
                    --".pdf"  ->  (template ++ ".tex", binWriter . (writePDF pdfCreator))
  templateData <- readFile template'
  case reader inputData of
        Right pandocData  -> writer templateData pandocData
        Left err          -> hPutStrLn stderr ("Conversion error:" ++ err) >> exitFailure
