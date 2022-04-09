-- | The 'Main' module contains the 'main' function which is the starting point of the program.
-- Besides the main function it also contains some helper function which handle the input arguments or handle the errors which can be raised during the execution of the program.
-- For every possible error that is raised detailed information about where the program ran into this error is given to the user.
module Main where

import qualified Data.Map as M (empty)
import GHC.IO.Exception (ExitCode)
import Language.Haskell.Interpreter as I (GhcError(..), InterpreterError(..))
import System.Environment (getArgs)
import System.FilePath (takeDirectory)

import MarkTeX.Evaluation.LatexGenerator (documentToLatex, ToLatexError(..))
import MarkTeX.Evaluation.MetaEvaluator (runEvaluation, Information(..), State(..), EvaluationError(..))
import MarkTeX.Parsing.Expression (ParseError(..))
import MarkTeX.Parsing.Parser (parseMd)
import MarkTeX.PdfGenerator (documentToPdf, PDFGenerationError(..))
import MarkTeX.ReadJson (readOptionalJson, ReadJsonError(..))
import MarkTeX.TemplateLang (TData)

-- | The 'MarkTexError' datatype encapsulates every possible error that can be raised during the execution of the program.
-- This datatype catches the errors of the different program steps, which may all result in some error.
-- (This datatype is currently not used, but may be used when overhauling the structure of the main function. This is currently done in a separate branch.) 
data MarkTexError = ParsingError ParseError
                  | ReadingDataError ReadJsonError
                  | EvaluatingError EvaluationError
                  | ConvertingToLaTeXError ToLatexError
                  | GeneratingPDFError PDFGenerationError

-- | The 'main' function is the starting point of the program. 
-- It asks the user for an input and output file name.
-- The possible input arguments are the input MarkTeX file name and the output file name of the PDF file.
-- The program takes the MarkTeX file and this is evaluated and eventually converted to a PDF file.
main :: IO ()
main = do

    -- Get the optional input file name and output file name
    args <- getArgs

    -- Determine the input and output file names from the user input
    let (mdFileName, pdfFileName) = handleArgs args

    -- Read the MarkTeX file into a String
    inputMd <- readFile mdFileName

    -- Execute the different steps of the process
    case parseMd inputMd of
        Left err -> handleParseError err
        Right rootExpr -> do
            jsonContents <- readOptionalJson "data/data.json"
            case jsonContents of
                Left err -> handleReadDataError err
                Right jsonData -> do
                    (State env info, evalResult) <- runEvaluation (takeDirectory mdFileName) rootExpr jsonData
                    case evalResult of
                        Left err -> handleEvaluationError err
                        Right rootExpr' -> do
                            putStrLn "Evaluated the templates in the markdown file!"
                            case documentToLatex rootExpr' (settings info) of
                                Left err -> handleLaTeXConversionError err
                                Right latexString -> do
                                    putStrLn "Interpreted the markdown to a LaTeX string!"
                                    pdfResult <- documentToPdf latexString (settings info) pdfFileName
                                    case pdfResult of
                                        Left err -> handlePDFGenerationError err
                                        Right () -> do
                                            putStrLn "Generated the PDF file from the LaTeX string!"
                                            putStrLn "The MarkTeX program was successfully executed!"


-- | The function 'handleArgs' determines whether a valid amount of arguments is passed to the 'MarkTeX' executable.
-- It expects two arguments, an input file name of a MarkTeX file and an output file name of the PDF file.
-- If only one argument is given it expects this to be the input file name and takes the default file name "output.pdf" for the output.
-- Furthermore if no arguments are given, the file name of the input file is expected to be "main.md".
handleArgs :: [String] -> (FilePath, FilePath)
handleArgs args =
    let
        -- TODO Also allow the .json file to be entered here
        defaultFileNameMD  = "main.md"
        defaultFileNamePDF = "output"
    in
        case args of
            [] ->
                (defaultFileNameMD, defaultFileNamePDF)
            [mdFileName] ->
                (mdFileName, defaultFileNamePDF)
            [mdFileName, pdfFileName] ->
                (mdFileName, pdfFileName)
            args ->
                error $ "Expected at most two arguments, but received " ++ show (length args) ++ " arguments!"

-- | The 'handleParseError' function prints the information to the user that the program failed during the parsing step.
handleParseError :: ParseError -> IO ()
handleParseError msg = do
    putStrLn "MarkTeX failed while parsing the input document!"
    printRaisedError msg

-- | The 'handleReadDataError' function prints the information to the user that the program failed in reading the initial json data.
handleReadDataError :: ReadJsonError -> IO ()
handleReadDataError err = do
    putStrLn "MarkTeX failed while reading the initial json data file!"
    handleReadDataError' err

-- | The 'handleReadDataError'' function prints the information to the user that the program failed while reading json data from a file.
handleReadDataError' :: ReadJsonError -> IO ()
handleReadDataError' err =
    case err of
        DecodeJson msg -> do
            putStrLn "The failure happened while interpreting the file contents as a JSON object."
            printRaisedError msg
        FileDoesNotExist path -> do
            putStrLn $ "The file on the path \"" ++ path ++ "\" does not exist or could not be found!"

-- | The 'handleEvaluationError' function prints the information to the user that the program failed while interpreting some template language code.
handleEvaluationError :: EvaluationError -> IO ()
handleEvaluationError err = do
    putStrLn "MarkTeX failed while evaluating the template language!"
    case err of 
        MetaCommandError msg -> do
            putStrLn "The failure happened because of an unexpected MetaCommand!"
            printRaisedError msg
        ExpectedWhile msg -> do
            putStrLn "The failure was raised because the evaluated MetaCommand is no longer a \"While\" constructor!"
            putStrLn "Make sure that a \"While\" MetaCommand always stays a \"While\" MetaCommand."
            printRaisedError msg
        ParseKeyError msg -> do
            putStrLn "Failed on parsing the specified key for looking up a value in the environment!"
            printRaisedError msg
        ReadDataError rje -> do
            putStrLn "The failure happened while reading additional json data!"
            handleReadDataError' rje
        InterpreterError ierr -> do
            putStrLn "The failure happened during the evaluation of a MetaCommand!"
            case ierr of
                UnknownError msg -> do
                    putStrLn "Failed due to an unknown error!"
                    printRaisedError msg
                NotAllowed msg -> do
                    putStrLn "The specified computation was not allowed!"
                    printRaisedError msg 
                GhcException msg -> do
                    putStrLn "A GHC exception was raised during evaluation!"
                    printRaisedError msg
                WontCompile ghcErrs -> do
                    putStrLn "The specified template string does not compile!"
                    putStrLn "Press Enter to see the GHC errors that were raised..."
                    _ <- getLine
                    putStrLn $ unlines $ map errMsg ghcErrs
                    
                
-- | The 'handleLaTeXConversionError' function prints the information to the user that the program failed while the MarkTeX expression is translated to a LaTeX string.
handleLaTeXConversionError :: ToLatexError -> IO ()
handleLaTeXConversionError err = do
    putStrLn "MarkTeX failed during the translation to LaTeX!"
    case err of 
        InvalidSectionLevel msg -> do
            putStrLn "The failure was raised due to an invalid section level!"
            printRaisedError msg
        ExpectedHyperlinkText msg -> do
            putStrLn "The failure was encountered because the url of a hyperlink was not given in plain text!"
            printRaisedError msg
        ExpectedImageText msg -> do
            putStrLn "The failure was encountered because the path to an image was not given in plain text!"
            printRaisedError msg

-- | The 'handlePDFGenerationError' function prints the information to the user that the program failed during the generation of the PDF.
handlePDFGenerationError :: PDFGenerationError -> IO ()
handlePDFGenerationError err = do
    putStrLn "MarkTex failed during the PDF generation step!"
    case err of
        PDFGenerationError n msg -> do
            putStrLn "The failure happened while generating the pdf from the LaTeX string!"
            printRaisedError msg
        PDFLaTeXNotFound msg -> do
            putStrLn "The failure happened while checking if \"pdflatex\" is installed!"
            printRaisedError msg

-- | The 'printRaisedError' function is a helper function which prints to the user that an error was raised together with the error message on the following line.
printRaisedError :: String -> IO ()
printRaisedError msg = putStrLn $ "The following error was raised:\n" ++ msg 
