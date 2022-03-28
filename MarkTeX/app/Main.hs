module Main where

import Parser (parseMd)
import PdfGenerator (documentToPdf)
import Language
import Templating.Generator

import qualified Data.Map as M (empty)
import GHC.IO.Exception (ExitCode)


-- | The `main` function takes a markdown file and converts it to a pdf file.
main :: IO ()
main = do

    -- Input and output file names
    let mdFileName = "main.md"
    let pdfFileName = "output.pdf"

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
            putStrLn $ "Evaluating the templates in the markdown file failed with the following error message:\n"
                     ++ show err
        -- On success continue with the returned state
        st@State {returnVal = evaluatedExpr, documentSettings = docSettings} -> do
            putStrLn "Evaluated the templates in the markdown file successfully!"
            let documentExpr = case evaluatedExpr of
                                    Left rootExpr -> rootExpr
                                    Right expr    -> Body expr
            documentToPdf rootExpr docSettings pdfFileName

