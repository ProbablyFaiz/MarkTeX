-- | The module `PdfGenerator` contains utility functions to convert a MarkDown AST to a latex or pdf file.
-- The `documentToPdf` function converts a `RootExpr` to a pdf file.
-- The `documentToLatex` and `latexToPdf` functions perform the intermediate steps of converting a `RootExpr` to a latex string and converting a latex file to a pdf file respectively.
module MarkTeX.PdfGenerator (documentToPdf, latexToPdf) where

import MarkTeX.Evaluation.Expression (Expr)
import MarkTeX.TemplateLang (TData)

import GHC.IO.Exception (ExitCode(..))
import System.Process (system)
import System.Directory (removeFile, createDirectoryIfMissing, removeDirectoryRecursive, renameFile)
import System.FilePath (joinPath)


-- | The `documentToPdf` function takes a LateX `String` together with certain document settings in a `TData` format and converts it to a pdf file.
documentToPdf :: String -> TData -> FilePath -> IO ()
documentToPdf latexString docSettings pdfFileName = do

    -- File name for intermediate tex state
    let tempDir = "tmp-marktex"
    -- Create the directory
    createDirectoryIfMissing False tempDir

    let tempTexFile = joinPath [tempDir, "tempFile.tex"]

    -- Write the LaTeX to the intermediate file
    writeFile tempTexFile latexString

    -- Convert the latex file to a pdf file with pdflatex
    exitCode <- latexToPdf tempDir tempTexFile pdfFileName

    -- Deletes the temp files 
    removeDirectoryRecursive tempDir

    -- Handle the value of the exit code
    handleExitCode exitCode
     
-- | The `latexToPdf` function converts a latex file into a pdf file.
latexToPdf :: FilePath -> FilePath -> FilePath -> IO ExitCode
latexToPdf outputDir texFile pdfFile = system $ pdfLatexCommand outputDir texFile pdfFile ++
                                                -- Move the pdf file to the outer directory if the compile succeeds
                                                " && " ++ mvOutputPdfCommand outputDir pdfFile
    where
        pdfLatexCommand :: FilePath -> FilePath -> FilePath -> String
        pdfLatexCommand out tex pdf = "pdflatex -output-directory=" ++ out ++ " -jobname=" ++ pdf ++ " " ++ tex

        mvOutputPdfCommand :: FilePath -> FilePath -> String
        mvOutputPdfCommand out pdf = let pdfPath = pdf ++ ".pdf" in "mv " ++ joinPath [out, pdfPath] ++ " " ++ pdfPath

-- | The `handleExitCode` function determines what to output to the user depending on the exit code of the system command that is ran on the command line.
handleExitCode :: ExitCode -> IO ()
handleExitCode exitCode = 
    case exitCode of
        ExitSuccess -> do
            putStrLn "Successfully converted the MarkDown to a pdf file!"
        ExitFailure n -> do
            putStrLn $ "Converting the MarkDown to a pdf file led to a failure with exit code " ++ show n ++ "\n"
                     ++ "Make sure that pdflatex is installed on your system."
            --TODO: possibly handle case distinction on n -> what are possible failure codes
