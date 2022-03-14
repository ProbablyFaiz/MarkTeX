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
documentToLaTeX es = "\\documentclass[12pt]{article}\n"
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
exprToLaTeX (OrderedList e)   = "\\begin{enumerate}\n" 
                             ++ exprToLaTeX e ++ "\n"
                             ++ "\\end{enumerate}\n"
exprToLaTeX (UnorderedList e) = "\\begin{itemize}\n"
                             ++ exprToLaTeX e ++ "\n"
                             ++ "\\end{itemize}\n"