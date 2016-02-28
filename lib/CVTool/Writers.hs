module CVTool.Writers (writePDF, writeLaTeX, writeHtml, writeJSON, writeMarkdown) where

import qualified Text.Pandoc as P (
  writeJSON,
  writeMarkdown,
  writeLaTeX,
  writeHtmlString, 
  WriterOptions(..))
import Text.Pandoc.PDF
import Text.CSL.Pandoc

defaultOptions = P.WriterOptions {
  P.writerStandalone = True,
  P.writerTableOfContents = False
}

injectTemplate template = defaultOptions { P.writerTemplate = template }

handleCitations pandocData = pandocData

writeText writer template pandocData = writer (injectTemplate template) $ handleCitations pandocData

writeJSON = writeText P.writeJSON
writeMarkdown = writeText P.writeMarkdown
writeLaTeX = writeText P.writeLaTeX
writeHtml = writeText P.writeHtmlString
writePDF template pdfCreator = makePDF pdfCreator P.writeLaTeX (injectTemplate template)
