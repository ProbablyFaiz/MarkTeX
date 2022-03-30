module LatexGenerator (documentToLatex) where

import Language (Expr'(..), RootExpr'(..))
import TemplateLang (TData)

-- Definition of document and such temporarily here

-- example document for testing
exampleDocument :: RootExpr'
exampleDocument = RootSeq' [Heading' 0 head1, body1, Heading' 1 head1_1, body1_1, Heading' 0 head2, Body' body2]
  where
    head1 = Bold' (Text' "Heading 1")
    body1 = OrderedList' [Italic' (Text' "Text in list")]
    head1_1 = Italic' (Text' "Heading 1.1")
    body1_1 = UnorderedList' [Text' "Text in list 1", Text' "Text in list 2"]
    head2 = Seq' [Text' "This is ", Italic' (Text' "Heading 2")]
    body2 = Seq' [Text' "Link to Google:", Hyperlink' (Text' "https://www.google.com/") (Text' "Google")]

documentToLatex :: RootExpr' -> TData -> String
documentToLatex re docSettings =
  "\\documentclass[12pt]{article}\n"
    ++ "\\usepackage{hyperref}\n"
    ++ "\\usepackage{graphicx}\n"
    ++ "\\begin{document}\n"
    ++ rootExprToLaTeX re
    ++ "\\end{document}\n"

rootExprToLaTeX :: RootExpr' -> String
rootExprToLaTeX (RootSeq' es) = concatMap rootExprToLaTeX es
rootExprToLaTeX (Heading' n e) = "\\" ++ repeatList n "sub" ++ "section{" ++ exprToLaTeX e ++ "}\n"
rootExprToLaTeX (Body' e) = exprToLaTeX e ++ "\n"
rootExprToLaTeX (OrderedList' e) =
  "\\begin{enumerate}\n"
    ++ concatMap exprToItem e
    ++ "\\end{enumerate}\n"
rootExprToLaTeX (UnorderedList' e) =
  "\\begin{itemize}\n"
    ++ concatMap exprToItem e
    ++ "\\end{itemize}\n"
rootExprToLaTeX NewLine' = "\n"

repeatList :: Int -> [a] -> [a]
repeatList n | n >= 1 && n <= 5 = concat . replicate (n - 1)
             | otherwise        = error "Invalid heading number!"

exprToLaTeX :: Expr' -> String
exprToLaTeX (Seq' es) = concatMap exprToLaTeX es
exprToLaTeX (Text' s) = s
exprToLaTeX (Bold' e) = "\\textbf{" ++ exprToLaTeX e ++ "}"
exprToLaTeX (Italic' e) = "\\textit{" ++ exprToLaTeX e ++ "}"
exprToLaTeX (Hyperlink' e (Text' url)) = "\\href{" ++ url ++ "}{" ++ exprToLaTeX e ++ "}"
exprToLaTeX (Image' e (Text' url)) =
  "\\begin{figure}\n"
    ++ "\\includegraphics{"
    ++ url
    ++ "}\n"
    ++ "\\caption{"
    ++ exprToLaTeX e
    ++ "}\n"
    ++ "\\end{figure}\n"
exprToLaTeX (Hyperlink' _ _) = error "Hyperlink must have a text URL"
exprToLaTeX (Image' _ _) = error "Image must have a text URL"

exprToItem :: Expr' -> String
exprToItem e = "\\item " ++ exprToLaTeX e ++ "\n"
