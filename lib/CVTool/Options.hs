module CVTool.Options (ToolOptions(..), toolOptionDescs, defaultOptions, printHelp) where

import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

data ToolOptions = ToolOptions {
  optHelp       :: Bool,
  optInFile     :: String,
  optOutFile    :: String
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
    \ and can be PDF, LaTeX, Markdown, JSON, or HTML. Defaults to cv.pdf"
  ]

defaultOptions = ToolOptions {
  optHelp       = False,
  optInFile     = "cv.yaml",
  optOutFile    = "cv.pdf"
}

printHelp = do
  prog <- getProgName
  hPutStrLn stderr (usageInfo (prog ++ header) toolOptionDescs)
  exitSuccess

  where header = ": Generate an academic CV from a conveniently formatted (e.g. YAML) data file"
