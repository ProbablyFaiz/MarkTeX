-- | The module 'PdfGenerator' contains utility functions to convert a latex file to a pdf file.
-- The exported 'documentToPdf' function converts a latex string to a pdf file.
-- Furthermore the 'PDFGenerationError' datatype is exported to such that a possible error can be handled.
module MarkTeX.PdfGenerator (documentToPdf, PDFGenerationError(..)) where

import MarkTeX.TemplateLang (TData)
import MarkTeX.TemplateLang.Expression (Expr)

import GHC.IO.Exception (ExitCode(..))
import System.Directory (removeFile, createDirectoryIfMissing, removeDirectoryRecursive, renameFile)
import System.FilePath (joinPath)
import System.Process (system)

-- | The 'PDFGenerationError' datatype contains the possible errors that can be raised during the generation of the PDF.
data PDFGenerationError = PDFGenerationError Int String
                        | PDFLaTeXNotFound String
    deriving (Show)

-- | The 'documentToPdf' function takes a LateX string together with certain document settings in a 'TData' format and converts it to a pdf file.
documentToPdf :: String -> TData -> FilePath -> IO (Either PDFGenerationError ())
documentToPdf latexString docSettings pdfFileName = do

    -- File name for intermediate tex state
    let tempDir = "tmp-marktex"

    -- Create the directory
    createDirectoryIfMissing False tempDir

    -- File path for the intermediate tex file
    let tempTexFile = joinPath [tempDir, "tempFile.tex"]

    -- Write the LaTeX to the intermediate file
    writeFile tempTexFile latexString

    -- Check if pdflatex is installed
    installedExitCode <- system "pdflatex --help > /dev/null"
    case installedExitCode of
        ExitFailure n -> 
            return $ Left $ PDFLaTeXNotFound "The command \"pdflatex\" was not found! Make sure that you have a LaTeX installation on your computer system to convert a LaTeX file to pdf format."
        ExitSuccess -> do
            -- Convert the latex file to a pdf file with pdflatex
            exitCode <- latexToPdf tempDir tempTexFile pdfFileName

            -- Delete the temporary files 
            removeDirectoryRecursive tempDir

            -- Handle the value of the exit code
            handleExitCode exitCode
     
-- | The 'latexToPdf' function converts a latex file into a pdf file by using pdflatex.
latexToPdf :: FilePath -> FilePath -> FilePath -> IO ExitCode
latexToPdf outputDir texFile pdfFile = system $ pdfLatexCommand outputDir texFile pdfFile ++
                                                -- Run pdfLaTeX again for cross-references etc.
                                                " && " ++ pdfLatexCommand outputDir texFile pdfFile ++
                                                -- Move the pdf file to the outer directory if the compile succeeds
                                                " && " ++ mvOutputPdfCommand outputDir pdfFile
    where
        pdfLatexCommand :: FilePath -> FilePath -> FilePath -> String
        pdfLatexCommand out tex pdf = "pdflatex -halt-on-error -output-directory=" ++ out ++ " -jobname=" ++ pdf ++ " " ++ tex ++ " > /dev/null"

        mvOutputPdfCommand :: FilePath -> FilePath -> String
        mvOutputPdfCommand out pdf = let pdfPath = pdf ++ ".pdf" in "mv " ++ joinPath [out, pdfPath] ++ " " ++ pdfPath

-- | The 'handleExitCode' function determines what to output to the user depending on the exit code of the system command that is ran on the command line.
-- If the exitcode points to a successful execution then () is returned which signals a successful execution.
handleExitCode :: ExitCode -> IO (Either PDFGenerationError ())
handleExitCode exitCode = 
    case exitCode of
        ExitSuccess ->
            return $ Right ()
        ExitFailure n ->
            return $ Left $ PDFGenerationError n $ "Converting the MarkDown to a pdf file led to a failure with exit code " ++ show n
