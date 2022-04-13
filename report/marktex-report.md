{{ docSetting "preambleStatements" "\\geometry{a4paper,margin=0.5in}" }}

(TODO: Add cover page support to MarkTeX or equivalent LaTeX commands)

Title: The MarkTeX document system

\tableofcontents

# Outline
-  Introduction

Combines best of Markdown, LaTeX, and Haskell 

Superset of Markdown

This report prepared using MarkTeX

- Reflection
 
Don't resist using idiomatic Haskell code

Initial implementation of the lexer used 


# Introduction

MarkTeX is a document preparation and generation system. It aims to combine the simplicity of Markdown with the expressive power of Haskell and the beautiful typography of LaTeX.

MarkTeX tries to make the basic features usable for those unfamiliar with Haskell while allowing advanced users to add their own logic through sub-templates and Haskell modules.

MarkTeX implements a superset of the ubiquitous Markdown markup language that supports Haskell-based templates as well as a wide array of other features.

In the spirit of eating our own dog food, this report has been prepared and generated using MarkTeX. The source files can be found [here](https://github.com/ProbablyFaiz/infoafp-project/tree/master/report).

# The Language

The MarkTeX language is a superset of the Markdown language.\footnote{The most authoritative specification of Markdown is the CommonMark spec. In reality, MarkTeX does not support *all* features set out by [Commonmark](https://spec.commonmark.org/0.30/): some features, such as raw HTML blocks are incompatible with the tool's purpose of generating PDF documents.}

```
# A sample MarkTeX document

MarkTeX supports all the usual features of Markdown, like
**bold** and *italic formatting*, as well as [hyperlinks]
(https://www.youtube.com/watch?v=dQw4w9WgXcQ).

MarkTeX also supports some unique features, like {{ templates }}
and even
{% block statements %}
More on these later.
{% end %}
```

# Template System

{{ Include "template-system.md" }}

# \TeX/PDF Conversion

MarkTeX's core implementation does not assume nor rely upon any eventual output format. Depending on intended usage, backends for MarkTeX's intermediate representation could be implemented for several formats, including HTML, Word, and even regular Markdown.

MarkTeX currently implements a backend for one output format: LaTeX-created PDF files. The MarkTeX IR is first transpiled to a LaTeX file which is then compiled to a PDF using PDFLaTeX.

# Bugs and limitations

MarkTeX is not perfect in its current state but it works well as a prototype. In order to make MarkTeX complete we would fix/add the following:

- Make parsing more robust, try combinations of all kinds of tags and fix the ones that aren't working properly.
- Add ~VarName~ as a shortcut for ```{{InsertVar "VarName"}}``` so that this much-used part becomes less verbose and doesn't require a call to the Haskell interpreter.
- Importing modules directly from the template is currently not possible, a future version could try to locate all package databases to fix this.
- It is currently possible to define custom components with the InsertExpr metacommand. These components currently only work for regular commands though, not for blocks. To make this complete we would need to add a way to create a function that operates on the content of a block in the form of an Expr. Then this Expr needs to be passed to the previously created function when intepreting it.
- Currently we support control over a few built-in LaTeX functionalities through Docsettings. Future versions should provide more control over built-in LaTeX functionalities.
- Evaluating Haskell code is quite slow since we don't re-use the interpreter and the environment module. To make MarkTeX suitable for large datasets this would need to be improved.

# Reflection

TODO
