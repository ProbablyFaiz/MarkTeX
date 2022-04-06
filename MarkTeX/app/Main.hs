module Main where

import qualified Data.Map as M (empty)
import GHC.IO.Exception (ExitCode)
import System.Environment (getArgs)
import System.FilePath (takeDirectory)

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

-- | The operator '(>>>=)' specifies how to chain 'IO (Either a b)' functions.
-- The input 'IO (Either a b)' argument is evaluated to a 'Either a b' datatype.
-- If this action failed, thus when this evaluates to the 'Left' constructor, this 'Left' constructor is returned.
-- Otherwise the second argument of type '(b -> IO (Either a c))' takes the value of the 'Right' constructor and performs the action of the second argument on this value.
(>>>=) :: IO (Either a b) -> (b -> IO (Either a c)) -> IO (Either a c)
a >>>= b = do
    a' <- a
    case a' of
      Left  l -> return $ Left l
      Right r -> b r

-- | The function 'mapLeft' maps a function on the 'Left' constructor of an 'Either' datatype.
-- If the 'Either' constructor is the 'Right' constructor, then this 'Right' constructor is returned.
mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left  x) = Left (f x)
mapLeft _ (Right x) = Right x

-- | For an 'IO' action which contains an 'Either' datatype, we map the 'mapLeft' on this 'Either' datatype.
-- A 'Left' constructor is mapped with the input function, while a 'Right' constructor is left untouched.
onFail :: IO (Either a c) -> (a -> b) -> IO (Either b c)
e `onFail` f = mapLeft f <$> e

-- | The function 'reorder' reorders an '(a, Either b c)' datatype to a 'Either b (a, c)' datatype.
reorder :: (a, Either b c) -> Either b (a, c)
reorder (x, e) = case e of
    Left l  -> Left l
    Right r -> Right (x, r)



documentToPdf2 :: String -> Settings -> String -> IO (Either PDFGenerationError ())
documentToPdf2 = undefined

pipeline :: String -> String -> String -> IO (Either MarkTexError ())
pipeline inputMd mdFileName pdfFileName =
    pure (parseMd inputMd) `onFail` ParsingError
        >>>= \rootExpr ->

    readOptionalJson "data/data.json" `onFail` ReadingDataError
        >>>= \jsonData ->
    
    fmap reorder (runEvaluation (takeDirectory mdFileName) rootExpr jsonData) `onFail` EvaluatingError
        >>>= \(State env info, expr) ->

    pure (documentToLatex expr (settings info)) `onFail` ConvertingToLaTeXError
        >>>= \latexString ->

    documentToPdf2 latexString (settings info) pdfFileName `onFail` GeneratingPDFError

        
handleParseError :: ParseError -> IO ()
handleParseError = print
handleReadDataError :: ReadJsonError -> IO ()
handleReadDataError = print
handleEvaluationError :: EvaluationError -> IO ()
handleEvaluationError = print
handleLaTeXConversionError :: ToLatexError -> IO ()
handleLaTeXConversionError = print
handlePDFGenerationError :: PDFGenerationError -> IO ()
handlePDFGenerationError = print
