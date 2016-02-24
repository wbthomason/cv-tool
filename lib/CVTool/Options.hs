module CVTool.Options (ToolOptions(..), toolOptionDescs, defaultOptions, printHelp) where

import System.Console.GetOpt
import System.Environment
import System.Exit
import System.FilePath
import System.IO

data ToolOptions = ToolOptions {
  optHelp       :: Bool,
  optInFile     :: FilePath,
  optOutFile    :: FilePath,
  optTemplate   :: FilePath
}

toolOptionDescs = [ 
  Option "h" ["help"] (NoArg (\opt -> opt { optHelp = True })) "Print this message",
  Option "if" ["in", "from"]
    (ReqArg (\arg opt -> opt { optInFile = arg }) "FILE")
    "The file containing the data for the CV. Format is inferred from the extension,\
    \ and can be YAML, JSON, or TOML. Defaults to cv.yaml",
  Option "ot" ["out", "to"]
    (ReqArg (\arg opt -> opt { optOutFile = arg }) "FILE")
    "The desired filename for the output CV. Format is inferred from the extension,\
    \ and can be PDF, LaTeX, Markdown, JSON, or HTML. Defaults to cv.pdf",
  Option "" ["template"] (ReqArg (\arg opt -> opt { optTemplate = arg }) "FILE") "The template file to use. Each output filetype has its own default."
  ]

defaultOptions = ToolOptions {
  optHelp       = False,
  optInFile     = "cv.yaml",
  optOutFile    = "cv.pdf",
  optTemplate   = "default.tex"
}

printHelp = do
  prog <- getProgName
  hPutStrLn stderr (usageInfo (prog ++ header) toolOptionDescs)
  exitSuccess

  where header = ": Generate an academic CV from a conveniently formatted (e.g. YAML) data file"
