{-# LANGUAGE LambdaCase #-}
-- | The 'Main' module contains the 'main' function which is the starting point of the program.
-- Besides the main function it also contains some helper function which handle the input arguments or handle the errors which can be raised during the execution of the program.
-- For every possible error that is raised detailed information about where the program ran into this error is given to the user.
module Main where

import Language.Haskell.Interpreter as I (GhcError(..), InterpreterError(..))
import System.FilePath (takeDirectory)

import MarkTeX.Evaluation.LatexGenerator (documentToLatex, ToLatexError(..))
import MarkTeX.Evaluation.MetaEvaluator (runEvaluation, Information(..), State(..), EvaluationError(..))
import MarkTeX.Parsing.Expression (ParseError)
import MarkTeX.Parsing.Parser (parseMd)
import MarkTeX.PdfGenerator (documentToPdf, PDFGenerationError(..))
import MarkTeX.ReadJson (readOptionalJson, ReadJsonError(..))
import Data.Either (isLeft)
import System.Directory.Internal.Prelude

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
    let (mdFileName, pdfFileName, jsonFileName) = handleArgs args

    -- Read the MarkTeX file into a String
    inputMd <- readFile mdFileName

    let rightUnsafe :: Either a b -> b 
        rightUnsafe = \case
            (Right x) -> x
            (Left _)  -> error "Error: Expected right"

    let leftUnsafe :: Either a b -> a
        leftUnsafe = \case
            (Left x)  -> x
            (Right _) -> error "Error: Expected left"

    -- Execute the different steps of the process
    do
        let parseResult = parseMd inputMd;
        when (isLeft parseResult) (handleParseError $ leftUnsafe parseResult);
        let rootExpr = rightUnsafe parseResult;

        jsonResult <- readOptionalJson jsonFileName;
        when (isLeft jsonResult) (handleReadDataError $ leftUnsafe jsonResult);
        let jsonData = rightUnsafe jsonResult;

        (State _ info, evalResult) <- runEvaluation (takeDirectory mdFileName) rootExpr jsonData;
        when (isLeft evalResult) (handleEvaluationError $ leftUnsafe evalResult);
        let evalExpr' = rightUnsafe evalResult;

        let latexResult = documentToLatex evalExpr' (settings info);
        when (isLeft latexResult) (handleLaTeXConversionError $ leftUnsafe latexResult);
        let latexString = rightUnsafe latexResult;

        pdfResult <- documentToPdf latexString (settings info) pdfFileName
        when (isLeft pdfResult) (handlePDFGenerationError $ leftUnsafe pdfResult);
        putStrLn ("Successfully generated " ++ pdfFileName);


-- | The function 'handleArgs' determines whether a valid amount of arguments is passed to the 'MarkTeX' executable.
-- It expects two mandatory arguments, an input file name of a MarkTeX file and an output file name of the PDF file.
-- The third argument is optional and specifies the location of a JSON file.
handleArgs :: [String] -> (FilePath, FilePath, Maybe FilePath)
handleArgs args = case args of
    [mdFileName, pdfFileName] ->
        (mdFileName, pdfFileName, Nothing)
    [mdFileName, pdfFileName, jsonFileName] ->
        (mdFileName, pdfFileName, Just jsonFileName)
    _ -> error "No input / output names were provided. Please add them to your command."

-- | The 'handleParseError' function prints the information to the user that the program failed during the parsing step.
handleParseError :: ParseError -> IO ()
handleParseError msg = do
    putStrLn "MarkTeX failed while parsing the input document."
    printRaisedError msg
    exitFailure;

-- | The 'handleReadDataError' function prints the information to the user that the program failed in reading the initial json data.
handleReadDataError :: ReadJsonError -> IO ()
handleReadDataError err = do
    putStrLn "MarkTeX failed while reading the initial json data file."
    handleReadDataError' err
    exitFailure;

-- | The 'handleReadDataError'' function prints the information to the user that the program failed while reading json data from a file.
handleReadDataError' :: ReadJsonError -> IO ()
handleReadDataError' err =
    case err of
        DecodeJson msg -> do
            putStrLn "The failure happened while interpreting the file contents as a JSON object."
            printRaisedError msg
            exitFailure;
        FileDoesNotExist path -> do
            putStrLn $ "The file on the path \"" ++ path ++ "\" does not exist or could not be found."
            exitFailure;

-- | The 'handleEvaluationError' function prints the information to the user that the program failed while interpreting some template language code.
handleEvaluationError :: EvaluationError -> IO ()
handleEvaluationError err = do
    putStrLn "MarkTeX failed while evaluating the template language."
    case err of 
        MetaCommandError msg -> do
            putStrLn "The failure happened because of an unexpected MetaCommand."
            printRaisedError msg
        ExpectedWhile msg -> do
            putStrLn "The failure was raised because the evaluated MetaCommand is no longer a \"While\" constructor."
            putStrLn "Make sure that a \"While\" MetaCommand always stays a \"While\" MetaCommand."
            printRaisedError msg
        ParseKeyError msg -> do
            putStrLn "Failed on parsing the specified key for looking up a value in the environment."
            printRaisedError msg
        ReadDataError rje -> do
            putStrLn "The failure happened while reading additional json data."
            handleReadDataError' rje
        InterpreterError ierr -> do
            putStrLn "The failure happened during the evaluation of a MetaCommand."
            case ierr of
                UnknownError msg -> do
                    putStrLn "Failed due to an unknown error."
                    printRaisedError msg
                NotAllowed msg -> do
                    putStrLn "The specified computation was not allowed."
                    printRaisedError msg 
                GhcException msg -> do
                    putStrLn "A GHC exception was raised during evaluation."
                    printRaisedError msg
                WontCompile ghcErrs -> do
                    putStrLn "The specified template string does not compile."
                    putStrLn "Press Enter to see the GHC errors that were raised..."
                    _ <- getLine
                    putStrLn $ unlines $ map errMsg ghcErrs
    exitFailure;
                    
                
-- | The 'handleLaTeXConversionError' function prints the information to the user that the program failed while the MarkTeX expression is translated to a LaTeX string.
handleLaTeXConversionError :: ToLatexError -> IO ()
handleLaTeXConversionError err = do
    putStrLn "MarkTeX failed during the translation to LaTeX."
    case err of 
        InvalidSectionLevel msg -> do
            putStrLn "The failure was raised due to an invalid section level."
            printRaisedError msg
        ExpectedHyperlinkText msg -> do
            putStrLn "The failure was encountered because the url of a hyperlink was not given in plain text."
            printRaisedError msg
        ExpectedImageText msg -> do
            putStrLn "The failure was encountered because the path to an image was not given in plain text."
            printRaisedError msg
    exitFailure;

-- | The 'handlePDFGenerationError' function prints the information to the user that the program failed during the generation of the PDF.
handlePDFGenerationError :: PDFGenerationError -> IO ()
handlePDFGenerationError err = do
    putStrLn "MarkTex failed during the PDF generation step."
    case err of
        PDFGenerationError _ msg -> do
            putStrLn "The failure happened while generating the pdf from the LaTeX string."
            printRaisedError msg
        PDFLaTeXNotFound msg -> do
            putStrLn "The failure happened while checking if \"pdflatex\" is installed."
            printRaisedError msg
    exitFailure;

-- | The 'printRaisedError' function is a helper function which prints to the user that an error was raised together with the error message on the following line.
printRaisedError :: String -> IO ()
printRaisedError msg = putStrLn $ "The following error was raised:\n" ++ msg 
