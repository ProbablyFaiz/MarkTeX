module Main where

import qualified Data.Map as M (empty)
import GHC.IO.Exception (ExitCode)
import System.Environment (getArgs)
import System.FilePath (takeDirectory)
import Language.Haskell.Interpreter as I (InterpreterError(..), GhcError(..))

import MarkTeX.Parsing.Parser (parseMd)
import MarkTeX.Evaluation.LatexGenerator (documentToLatex, ToLatexError(..))
import MarkTeX.Evaluation.MetaEvaluator (runEvaluation, Information(..), State(..), EvaluationError(..))
import MarkTeX.PdfGenerator (documentToPdf, PDFGenerationError(..))
import MarkTeX.ReadJson (readOptionalJson, ReadJsonError(..))
import MarkTeX.Parsing.Expression (ParseError(..))
import MarkTeX.TemplateLang (TData)
import MarkTeX (Settings)

-- | 'MarkTexError'
data MarkTexError = ParsingError ParseError
                  | ReadingDataError ReadJsonError
                  | EvaluatingError EvaluationError
                  | ConvertingToLaTeXError ToLatexError
                  | GeneratingPDFError PDFGenerationError

-- | The 'main' function takes a markdown file and converts it to a pdf file.
-- Arguments are markdown file full file name and output file name of pdf file which receives .pdf extension later on.
main :: IO ()
main = do

    -- Get input file name and possibly output file name
    args <- getArgs

    -- Input and output file names
    let (mdFileName, pdfFileName) = handleArgs args

    -- Read Markdown File into string
    inputMd <- readFile mdFileName

    -- Evaluate the template parts in the AST
    case parseMd inputMd of
        Left err -> handleParseError err
        Right rootExpr -> do
            jsonContents <- readOptionalJson "data/data.json"
            case jsonContents of
                Left err -> handleReadDataError err
                Right jsonData -> do
                    (State env info, evalResult) <- runEvaluation (takeDirectory mdFileName) rootExpr jsonData
                    print (settings info)
                    case evalResult of
                        Left err -> handleEvaluationError err
                        Right rootExpr' -> do
                            putStrLn "Evaluated the templates in the markdown file!"
                            case documentToLatex rootExpr' (settings info) of
                                Left err -> handleLaTeXConversionError err
                                Right latexString -> do
                                    putStrLn "Interpreted the markdown to a LaTeX string!"
                                    documentToPdf latexString (settings info) pdfFileName
                                    print jsonData


-- | This function 'handleArgs' determines whether a valid amount of arguments is passed to the 'MarkTeX' executable.
-- It expects two arguments, an input file name of a markdown file and an output file name of the pdf file, where the markdown is converted to pdf format.
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

-- Handling all possible errors

handleParseError :: ParseError -> IO ()
handleParseError msg = do
    putStrLn "MarkTeX failed while parsing the input document!"
    printRaisedError msg

handleReadDataError :: ReadJsonError -> IO ()
handleReadDataError err = do
    putStrLn "MarkTeX failed while reading the initial json data file!"
    handleReadDataError' err

handleReadDataError' :: ReadJsonError -> IO ()
handleReadDataError' err =
    case err of
        DecodeJson msg -> do
            putStrLn "The failure happened while interpreting the file contents as a JSON object."
            printRaisedError msg
        FileDoesNotExist path -> do
            putStrLn $ "The file on the path \"" ++ path ++ "\" does not exist or could not be found!"


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

handlePDFGenerationError :: PDFGenerationError -> IO ()
handlePDFGenerationError err = do
    putStrLn "MarkTex failed during the PDF generation step!"
    case err of
        PDFGenerationError msg -> do
            putStrLn "The failure happened while generating the pdf from the LaTeX string!"
            printRaisedError msg
        PDFLaTeXNotFound msg -> do
            putStrLn "The failure happened while checking if \"pdflatex\" is installed!"
            printRaisedError msg

printRaisedError :: String -> IO ()
printRaisedError msg = putStrLn $ "The following error was raised:\n" ++ msg 
