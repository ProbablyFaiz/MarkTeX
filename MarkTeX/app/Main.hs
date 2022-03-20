module Main where

import PdfGenerator (latexToPdf)

main :: IO ()
main = do 
    let texFile = "TestFiles/document.tex"
        pdfFile = "TestFiles/document.pdf" 

    exitCode <- latexToPdf texFile pdfFile

    print exitCode
