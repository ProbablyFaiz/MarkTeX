-- | The 'MarkTeX.Evaluation.LatexGenerator' module contains the functionality for converting a Markdown AST to a LaTeX string.
-- It exports the 'documentToLatex' which does this conversion and returns the output string.
-- Besides that it exports the 'ToLatexError' datatype which can be returned by the 'documentToLatex' function if an error is raised.
module MarkTeX.Evaluation.LatexGenerator (documentToLatex, ToLatexError(..)) where

import MarkTeX.TemplateLang (TData, lookupTData, toString)
import MarkTeX.TemplateLang.Expression (Expr(..))


----- Data types -----


-- | The 'ToLatexError' datatype contains all errors that can occur when converting the Markdown AST into a LaTeX string.
data ToLatexError = InvalidSectionLevel String
                  | ExpectedHyperlinkText String
                  | ExpectedImageText String
    deriving (Show)

-- | The 'ToLatex' datatype contains the result of converting the Markdown AST into a LaTeX string.
-- When the conversion fails a 'ToLatexError' is raised, otherwise on success it returns a LaTeX string.
type ToLatex = Either ToLatexError String


----- Functions for combining normal strings and ToLatex strings -----


-- These operators bind to the left just like (++) and have a higher priority than (++)
infixl 4 <++>
infixl 4 ++>
infixl 4 <++

-- | The operator '<++>' evaluates two 'ToLatex' operations and concatenates their results.
(<++>) :: ToLatex -> ToLatex -> ToLatex
(<++>) a b = (++) <$> a <*> b

-- | The operator '++>' evaluates a 'ToLatex' operation and prepends the first argument, a simple String, to its result.
(++>) :: String -> ToLatex -> ToLatex
(++>) a b = (++) a <$> b

-- | The operator '<++' evaluates a 'ToLatex' operation and appends the second argument, a simple String, to its result.
(<++) :: ToLatex -> String -> ToLatex
(<++) a b = (++) <$> a <*> pure b


----- Functions for converting a full Markdown AST into a LaTeX string -----

-- | The 'documentToLatex' function adds the outer LaTeX layout, together with optional document settings, to the LaTeX string before converting the given Markdown AST. 
documentToLatex :: Expr -> TData -> ToLatex
documentToLatex re docSettings =
  "\\documentclass[12pt]{article}\n"
    ++ geometryConfig docSettings
    ++ "\\usepackage{hyperref}\n"
    ++ "\\usepackage{graphicx}\n"
    ++ stringDocSetting "preambleStatements" docSettings ++ "\n"
    ++ "\\begin{document}\n"
    ++> exprToLaTeX docSettings re
    <++ "\\end{document}\n"

-- | The 'stringDocSetting' function retrieves a document setting from the document settings.
stringDocSetting :: String -> TData -> String
stringDocSetting setting docSettings = toString $ lookupTData setting docSettings

-- | The 'geometryConfig' function gives optional arguments to the import of the LaTeX package geometry.
geometryConfig :: TData -> String
geometryConfig docSettings =
  "\\usepackage["
  ++ stringDocSetting "geometryConfig" docSettings
  ++ "]{geometry}\n"

-- | The 'exprToLaTeX' function converts an 'Expr' into a LaTeX string.
-- An error is raised if the url of a hyperlink or the path to an image is not in plain text.
-- When an invalid heading number is given, this also results in an error.
exprToLaTeX :: TData -> Expr -> ToLatex
exprToLaTeX ds (Heading n e) =
  "\\"
    ++> sectionLevel n
    <++ "{"
    <++> exprToLaTeX ds e
    <++ "}"
exprToLaTeX ds (OrderedList es) =
  "\\begin{enumerate}\n"
    ++> traverseAndCollect (exprToItem ds) es
    <++ "\\end{enumerate}\n"
exprToLaTeX ds (UnorderedList es) =
  "\\begin{itemize}\n"
    ++> traverseAndCollect (exprToItem ds) es
    <++ "\\end{itemize}\n"
exprToLaTeX _ NewLine = pure "\n"
exprToLaTeX ds (Seq es) = traverseAndCollect (exprToLaTeX ds) es
exprToLaTeX _ (Text s) = pure s
exprToLaTeX ds (Bold e) =
  "\\textbf{"
    ++> exprToLaTeX ds e
    <++ "}"
exprToLaTeX ds (Italic e) =
  "\\textit{"
    ++> exprToLaTeX ds e
    <++ "}"
exprToLaTeX ds (Hyperlink e (Text url)) =
  "\\href{"
    ++ url
    ++ "}{"
    ++> exprToLaTeX ds e
    <++ "}"
exprToLaTeX ds (Image e (Text url)) =
  "\\begin{figure}\n"
    ++ "\\includegraphics["
    ++ stringDocSetting "imageFormatConfig" ds
    ++ "]{"
    ++ url
    ++ "}\n"
    ++ "\\caption{"
    ++> exprToLaTeX ds e
    <++ "}\n"
    ++ "\\end{figure}\n"
exprToLaTeX _ (CodeSnippet s) = pure $ "\\begin{verbatim}" ++ s ++ "\\end{verbatim}\n"
exprToLaTeX _ (Hyperlink _ _) = Left $ ExpectedHyperlinkText "The url of a hyperlink should be given in plain text!"
exprToLaTeX _ (Image     _ _) = Left $ ExpectedImageText     "The path to an image should be given in plain text!"

-- | The function 'exprToItem' converts the given expression to a LaTeX string.
-- Then it makes an item for a ordered or unordered list from this string.
exprToItem :: TData -> Expr -> ToLatex
exprToItem ds e = "\\item " ++> exprToLaTeX ds e <++ "\n"


----- Small helper functions -----


-- | 'traverseAndCollect' is a helper function which traverses a list of expressions and maps every expression to a LaTeX string.
-- Then the list of LaTeX strings are concatenated to a single LaTeX string.
traverseAndCollect :: (a -> Either ToLatexError String) -> [a] -> Either ToLatexError String
traverseAndCollect f = fmap concat . traverse f

-- | 'sectionLevel' prepends the string "sub" a number of times before the string "section" to make sections of different levels.
-- The expected level of a section should be between 1 and 5.
-- If the nesting level is out of bounds, it raises a 'InvalidSectionLevel' error.
sectionLevel :: Int -> ToLatex
sectionLevel n | n >= 1 && n <= 5 = Right $ concat (replicate (n - 1) "sub") ++ "section"
               | otherwise        = Left  $ InvalidSectionLevel "Heading number is out of bounds!"
