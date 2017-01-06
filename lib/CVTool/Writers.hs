module CVTool.Writers (writePDF, writeLaTeX, writeHtml, writeJSON, writeMarkdown) where

import qualified Text.Pandoc as P (
  writeJSON,
  writeMarkdown,
  writeLaTeX,
  writeHtmlString,
  WriterOptions(..),
  def)
import Text.Pandoc.PDF

setOptions template = P.def { P.writerTemplate = template, P.writerStandalone = True }

handleCitations pandocData = pandocData

writeText writer template pandocData = writer (setOptions template) $ handleCitations pandocData

writeJSON = writeText P.writeJSON
writeMarkdown = writeText P.writeMarkdown
writeLaTeX = writeText P.writeLaTeX
writeHtml = writeText P.writeHtmlString
writePDF pdfCreator template = makePDF pdfCreator P.writeLaTeX (setOptions template)
