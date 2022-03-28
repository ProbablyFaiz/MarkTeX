module Main where

import Parser (parseMd)
import PdfGenerator (documentToPdf)
import Language (RootExpr(..))
import Templating.Generator (GeneratorState(..), getRootExpr, evalRootExpr)

import qualified Data.Map as M (empty)
import GHC.IO.Exception (ExitCode)
import System.Environment (getArgs)


-- | The `main` function takes a markdown file and converts it to a pdf file.
-- Arguments are markdown file full file name and output file name of pdf file which receives .pdf extension later on.
main :: IO ()
main = do

    -- Get input file name and possibly output file name
    args <- getArgs

    -- Input and output file names
    let (mdFileName, pdfFileName) = handleArgs args

    -- Read Markdown File into string
    inputMd <- readFile mdFileName

    -- Parse Markdown string into RootExpr AST
    let rootExpr = parseMd inputMd

    -- Evaluate the template parts in the AST
    let initData = M.empty -- Optionally read json or something instead of empty map
    generatedState <- evalRootExpr rootExpr initData

    -- Continue based on success of the generator state
    case generatedState of
        -- On error return that it failed to evaluate the template parts
        Error err -> do
            error $ "Evaluating the templates in the markdown file failed with the following error message:\n"
                     ++ show err
        -- On success continue with the returned state
        st@State {returnVal = evaluatedExpr, documentSettings = docSettings} -> do
            putStrLn "Evaluated the templates in the markdown file successfully!"
            let documentExpr = getRootExpr evaluatedExpr
            documentToPdf documentExpr docSettings pdfFileName

-- | This function `handleArgs` determines whether a valid amount of arguments is passed to the `MarkTeX` executable.
-- It expects two arguments, an input file name of a markdown file and an output file name of the pdf file, where the markdown is converted to pdf format.
-- If only one argument is given it expects this to be the input file name and takes the default file name "output.pdf" for the output.
-- Furthermore if no arguments are given, the file name of the input file is expected to be "main.md".
handleArgs :: [String] -> (FilePath, FilePath)
handleArgs args = 
    let 
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
                