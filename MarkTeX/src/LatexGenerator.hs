module LatexGenerator (documentToLatex, ToLatexError(..)) where

import Language (Expr'(..), RootExpr'(..))
import TemplateLang (TData)


----- Data types -----


-- | The `ToLatexError` datatype contains all errors that can occur when converting the MarkDown AST into a LaTeX string.
data ToLatexError = InvalidSectionLevel String
                  | ExpectedHyperlinkText String
                  | ExpectedImageText String
    deriving (Show)

-- | The `ToLatex` datatype contains the result of converting the MarkDown AST into a LaTeX string.
-- When the conversion fails a `ToLatexError` is raised, otherwise it returns a LaTeX string.
type ToLatex = Either ToLatexError String


----- Functions for combining normal strings and LaTeX strings -----


-- These operators bind to the left and have a higher priority than (++)
infixl 4 <++>
infixl 4 ++>
infixl 4 <++

-- | The operator `<++>` evaluates two `ToLatex` operations and then concatenates their results.
(<++>) :: ToLatex -> ToLatex -> ToLatex
(<++>) a b = (++) <$> a <*> b

-- | The operator `++>` evaluates a `ToLatex` operation and then prepends a string to its result.
(++>) :: String -> ToLatex -> ToLatex
(++>) a b = (++) a <$> b

-- | The operator `<++` evaluates a `ToLatex` operation and then appends a string to its result.
(<++) :: ToLatex -> String -> ToLatex
(<++) a b = (++) <$> a <*> pure b


----- Functions for converting a full MarkDown AST into a LaTeX string -----

-- | The `documentToLatex` function adds the outer LaTeX layout to the LaTeX string before converting the given `RootExpr'`. 
documentToLatex :: RootExpr' -> TData -> ToLatex
documentToLatex re docSettings =
  "\\documentclass[12pt]{article}\n"
    ++ "\\usepackage{hyperref}\n"
    ++ "\\usepackage{graphicx}\n"
    ++ "\\begin{document}\n"
    ++> rootExprToLaTeX re
    <++ "\\end{document}\n"

-- | The `rootExprToLaTeX` function converts a `RootExpr'` into a LaTeX string.
rootExprToLaTeX :: RootExpr' -> ToLatex
rootExprToLaTeX (RootSeq' es) = traverseAndCollect rootExprToLaTeX es
rootExprToLaTeX (Heading' n e) = 
  "\\" 
    ++> sectionLevel n
    <++ "{" 
    <++> exprToLaTeX e 
    <++ "}"
rootExprToLaTeX (Body' e) = exprToLaTeX e
rootExprToLaTeX (OrderedList' es) =
  "\\begin{enumerate}\n"
    ++> traverseAndCollect exprToItem es
    <++ "\\end{enumerate}\n"
rootExprToLaTeX (UnorderedList' es) =
  "\\begin{itemize}\n"
    ++> traverseAndCollect exprToItem es
    <++ "\\end{itemize}\n"
rootExprToLaTeX NewLine' = pure "\n"

-- | The `rootExprToLaTeX` function converts a `RootExpr'` into a LaTeX string.
-- An error is raised if the url of a hyperlink or the path to an image is not in plain text.
exprToLaTeX :: Expr' -> ToLatex
exprToLaTeX (Seq' es) = traverseAndCollect exprToLaTeX es
exprToLaTeX (Text' s) = pure s
exprToLaTeX (Bold' e) = 
  "\\textbf{"
    ++> exprToLaTeX e 
    <++ "}"
exprToLaTeX (Italic' e) = 
  "\\textit{"
    ++> exprToLaTeX e 
    <++ "}"
exprToLaTeX (Hyperlink' e (Text' url)) = 
  "\\href{" 
    ++ url 
    ++ "}{" 
    ++> exprToLaTeX e 
    <++ "}"
exprToLaTeX (Image' e (Text' url)) =
  "\\begin{figure}\n"
    ++ "\\includegraphics{"
    ++ url
    ++ "}\n"
    ++ "\\caption{"
    ++> exprToLaTeX e
    <++ "}\n"
    ++ "\\end{figure}\n"
exprToLaTeX (Hyperlink' _ _) = Left $ ExpectedHyperlinkText "The url of a hyperlink should be given in plain text!"
exprToLaTeX (Image'     _ _) = Left $ ExpectedImageText     "The path to an image should be given in plain text!"

-- | The function `exprToItem` converts the given expression to a LaTeX string.
-- Then it makes an item for a ordered or unordered list from this string.
exprToItem :: Expr' -> ToLatex
exprToItem e = "\\item " ++> exprToLaTeX e <++ "\n"


----- Small helper functions -----


-- | `traverseAndCollect` is a helper function which traverses a list of expressions and maps every expression to a LaTeX string.
-- Then the list of LaTeX strings are concatenated to a single LaTeX string.
traverseAndCollect :: (a -> Either ToLatexError String) -> [a] -> Either ToLatexError String
traverseAndCollect f = fmap concat . traverse f

-- | `sectionLevel` prepends the string "sub" a number of times before the string "section" to make sections of different levels.
-- The expected level of a section should be between 1 and 5.
-- If the nesting level is out of bounds, it raises a `InvalidSectionLevel` error.
sectionLevel :: Int -> ToLatex
sectionLevel n | n >= 1 && n <= 5 = Right $ concat (replicate (n - 1) "sub") ++ "section"
               | otherwise        = Left  $ InvalidSectionLevel "Heading number is out of bounds!"




-- example document for testing
exampleDocument :: RootExpr'
exampleDocument = RootSeq' [Heading' 1 head1, body1, Heading' 2 head1_1, body1_1, Heading' 1 head2, Body' body2]
  where
    head1 = Bold' (Text' "Heading 1")
    body1 = OrderedList' [Italic' (Text' "Text in list")]
    head1_1 = Italic' (Text' "Heading 1.1")
    body1_1 = UnorderedList' [Text' "Text in list 1", Text' "Text in list 2"]
    head2 = Seq' [Text' "This is ", Italic' (Text' "Heading 2")]
    body2 = Seq' [Text' "Link to Google:", Hyperlink' (Text' "https://www.google.com/") (Text' "Google")]