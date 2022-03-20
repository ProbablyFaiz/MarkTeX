module PdfGenerator( 
  latexToPdf
) where

import GHC.IO.Exception (ExitCode)

import System.Process (system)

latexToPdf :: FilePath -> FilePath -> IO ExitCode
latexToPdf texFile pdfFile = system $ pdfLatexCommand texFile pdfFile

pdfLatexCommand :: FilePath -> FilePath -> String
pdfLatexCommand tex pdf = "pdflatex " ++ tex ++ " " ++ pdf