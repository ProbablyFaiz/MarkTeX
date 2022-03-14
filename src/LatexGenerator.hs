module LatexGenerator (documentToLaTeX) where

-- Definition of document and such temporarily here
type Document = [RootExpr]

data RootExpr = Heading Int Expr
              | Body Expr

data Expr = Sequence [Expr]
          | Text String
          | Bold Expr
          | Italic Expr
          | Hyperlink String Expr
          | Image String Expr
          | OrderedList Expr
          | UnorderedList Expr

-- example document for testing
exampleDocument :: Document
exampleDocument = [Heading 0 head1, Body body1, Heading 1 head1_1, Body body1_1, Heading 0 head2,  Body body2]
    where
        head1 = Bold (Text "Heading 1")
        body1 = OrderedList (Italic (Text "Text in list"))
        head1_1 = Italic (Text "Heading 1.1")
        body1_1 = UnorderedList (Sequence [Text "Text in list 1", Text "Text in list 2"])
        head2 = Sequence [Text "This is ", Italic (Text "Heading 2")]
        body2 = Sequence [Text "Link to Google:", Hyperlink "https://www.google.com/" (Text "Google")]


documentToLaTeX :: Document -> String
documentToLaTeX es = "\\documentclass[12pt]{article}\n"
                  ++ "\\usepackage{hyperref}\n"
                  ++ "\\begin{document}\n" 
                  ++ concatMap rootExprToLaTeX es
                  ++ "\\end{document}\n"

rootExprToLaTeX :: RootExpr -> String
rootExprToLaTeX (Heading n e) = "\\" ++ repeatList n "sub" ++ "section{" ++ exprToLaTeX e ++ "}\n"
rootExprToLaTeX (Body      e) = exprToLaTeX e ++ "\n"

repeatList :: Int -> [a] -> [a]
repeatList n = concat . replicate n

exprToLaTeX :: Expr -> String
exprToLaTeX (Sequence es)     = concatMap exprToLaTeX es
exprToLaTeX (Text s)          = s
exprToLaTeX (Bold e)          = "\\textbf{" ++ exprToLaTeX e ++ "}"
exprToLaTeX (Italic e)        = "\\textit{" ++ exprToLaTeX e ++ "}"
exprToLaTeX (Hyperlink s e)   = "\\href{" ++ s ++ "}{" ++ exprToLaTeX e ++ "}\n"
exprToLaTeX (Image s e)       = "\\begin{figure}\n"
                             ++ "\\includegraphics{" ++ s ++ "}\n"
                             ++ "\\caption{" ++ exprToLaTeX e ++ "}\n" 
                             ++ "\\end{figure}\n"

exprToLaTeX (OrderedList   (Sequence es)) = "\\begin{enumerate}\n" 
                                         ++ concatMap (\e -> "\\item " ++ exprToLaTeX e ++ "\n") es
                                         ++ "\\end{enumerate}\n"
exprToLaTeX (UnorderedList (Sequence es)) = "\\begin{itemize}\n"
                                         ++ concatMap (\e -> "\\item " ++ exprToLaTeX e ++ "\n") es
                                         ++ "\\end{itemize}\n"

exprToLaTeX (OrderedList e)   = "\\begin{enumerate}\n" 
                             ++ "\\item " ++ exprToLaTeX e ++ "\n"
                             ++ "\\end{enumerate}\n"
exprToLaTeX (UnorderedList e) = "\\begin{itemize}\n"
                             ++ "\\item " ++ exprToLaTeX e ++ "\n"
                             ++ "\\end{itemize}\n"
