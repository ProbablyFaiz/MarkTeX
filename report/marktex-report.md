{{ docSetting "preambleStatements" "\\geometry{a4paper,margin=1.5in}" }}

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

MarkTeX implements a superset of the ubiquitous Markdown markup language that supports Haskell-based templates as well as a wide array of other features.

In the spirit of eating our own dog food, this report has been prepared and generated using MarkTeX. The source files can be found [here](https://github.com/ProbablyFaiz/infoafp-project/tree/master/report).

# Architecture

MarkTeX consists of three main components: the eponymous MarkTeX language, a Haskell-based templating system, and a TeX backend for creating PDF documents. We describe each of these components in turn.


## The Language

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


## Template System

The idea of the template system is to be able to execute Haskell code to insert data, create subcomponents and to control the eventual look of the document.

The template language consists of a range of metacommands. 
These metacommands can either be simple metacommands or metablocks.Simple metacommands are enclosed by double brackets, e.g. *\{\{ set ``x" 0 \}\}* to set the value of a variable named *x* to $0$. 
The simple metacommands can be used inline. 
The metablocks on the other hand cannot be used inline and span over multiple lines. 
These metablocks are control structures such as conditionals and loops. They start with a metacommand enclosed by brackets and percentage signs on the first line, for example *\{\% ifTrue (get ``x" == 0) \%\}* for a conditional block on whether the value of the variable *x* is $0$. 
Following this metacommand these metablocks contain some body spanning the following lines and finally they end with the end tag *\{\% end \%\}* on a separate line.

There are many supported metacommands. The metacommands used in metablocks are either conditionals or loops, which must be one of the following:

- *If TValue, IfVar String*
- *For String TValue, While TValue*

All other metacommands are simple metacommands, these other metacommands are:

- *Insert TValue, InsertVar String, InsertExpr Expr*
- *SetVar String TValue*
- *DocSetting String TValue, DocSettings TData*
- *LoadHsFile String, Import String, ImportQ String String*
- *Include String, IncludeWith String TData*
- *ReadJson String, ReadJsonQ String String*

The `TValue' and `TData' arguments to the Metacommand constructors can be given any data that can be converted to a `TValue' or `TData'. There are exposed functions *ifTrue*, *ifFalse*, *insert*, *docSetting*, *docSettings*, *set*, *for* and *while* that make this possible. They take any value which can be converted to a `TValue' or `TData' and construct a valid Metacommand from the given input.

## \TeX/PDF Backend

MarkTeX's core implementation does not assume nor rely upon any eventual output format. Depending on intended usage, backends for MarkTeX's intermediate representation could be implemented for several formats, including HTML, Word, and even regular Markdown.

MarkTeX currently implements a backend for one output format: LaTeX-created PDF files. The MarkTeX IR is first transpiled to a LaTeX file which is then compiled to a PDF using PDFLaTeX.

# Reflection

TODO
