module LatexGenerator where

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


documentToLaTeX :: Document -> String
documentToLaTeX es = "\\begin{document}" ++ concatMap rootExprToLaTeX es ++ "\\end{document}"

rootExprToLaTeX :: RootExpr -> String
rootExprToLaTeX (Heading n e) = "\\" ++ concat (replicate n "sub") ++ "section{" ++ exprToLaTeX e ++ "}\n"
rootExprToLaTeX (Body      e) = exprToLaTeX e

exprToLaTeX :: Expr -> String
exprToLaTeX (Sequence es)     = concatMap exprToLaTeX es
exprToLaTeX (Text s)          = s
exprToLaTeX (Bold e)          = "\\textbf{" ++ exprToLaTeX e ++ "}"
exprToLaTeX (Italic e)        = "\\textit{" ++ exprToLaTeX e ++ "}"
exprToLaTeX (Hyperlink s e)   = "\\href{" ++ s ++ "}{" ++ exprToLaTeX e ++ "}\n"
exprToLaTeX (Image s e)       = "\\begin{figure}\n\\includegraphics{" ++ s ++ "}\n\\caption{" ++ exprToLaTeX e ++ "}\\end{figure}"
exprToLaTeX (OrderedList e)   = "\\begin{enumerate}\n" ++ exprToLaTeX e ++ "\\end{enumerate}\n"
exprToLaTeX (UnorderedList e) = "\\begin{itemize}\n" ++ exprToLaTeX e ++ "\\end{itemize}\n"