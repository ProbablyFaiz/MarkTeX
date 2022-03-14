module PdfGenerator( 
  someFunc
) where

import Control.Applicative

import Control.Monad ((>=>))

import Data.Text (pack, unpack, Text(..))
import Data.Text.Encoding (decodeUtf8)

import Data.ByteString (ByteString(..))
import Data.ByteString.Lazy as BL (toStrict, ByteString(..))

import System.IO (readFile, writeFile, IOMode(..))

import Text.Pandoc (readLaTeX, writeLaTeX, runIO, Pandoc(..), PandocIO(..), ReaderOptions(..))
import Text.Pandoc.PDF (makePDF)
import Text.Pandoc.Options (def)

-- This did not really work actually
-- Better to use the command line tool pandoc
-- Actually then we might as well just use pdflatex, because pandoc uses this under the hood
-- Besides that pandoc does not work 100% correctly when converting tex to pdf

generatePdf :: IO ()
generatePdf =
    do putStrLn "Reading file..."
       latexInput <- readFile "main.tex"
       putStrLn "Converting latex to pdf..."
       pandocOutput <- runIO . latexToPdf $ pack latexInput
       case pandocOutput of
            Left pandocError ->
              do putStrLn "An error occured while running the pandoc conversion!"
            Right pdfOutput -> 
              case pdfOutput of
                Left pdfError -> 
                  do putStrLn "An error occured while making the PDF file!"
                     print pdfError
                Right pdfString ->
                  do putStrLn "Succesfully converted latex to pdf!"
                     writeFile "main.pdf" $ (unpack . decodeUtf8 . toStrict) pdfString

latexToPdf :: Text -> PandocIO (Either BL.ByteString BL.ByteString)
latexToPdf = latexToPandoc >=> pandocToPdf

latexToPandoc :: Text -> PandocIO Pandoc
latexToPandoc = readLaTeX def

pandocToPdf :: Pandoc -> PandocIO (Either BL.ByteString BL.ByteString)
pandocToPdf = makePDF "pdflatex" [] writeLaTeX def



someFunc :: IO ()
someFunc = putStrLn "someFunc"
