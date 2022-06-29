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

Metacommands are currently evaluated using the `hint' library. To make the JSON data available within the template we generate a temporary Haskell file that contains the environment plus some utility functions like `get'. 
This temporary haskell file is then imported into the context. Currently everything works well except for module imports, the `hint' library doesn't automatically detect the available package databases which means these modules can't be found.
The current workaround for this is to create a Haskell file, import the neccesary modules and re-export them. This Haskell file can then be imported using the `LoadHsFile' command.

`TValue' is defined as **data** TValue = TString **String** | TNumber **Float** | TBool **Bool** | TList **`[TValue]`** | TData **TData** | TNull.
This closely represents the JSON datatype which is the data that we will be working with.
In order to keep the Haskell code within the template from becoming very verbose we applied a few tricks:

- TValue has instances for Ord, Eq, Num, Fractional and Real. This allows basic operators to work between TValue and TValue and between TValue and numbers.
- TValue has an IsList instance which allows list-like pattern matching. We've also overridden the (++) operator to work with both TValue and List interchangably.
- We've overridden the Boolean operators to work with both TValue and Bool interchangably. All values of TValue are mapped to boolean values, e.g. an empty List is mapped to False.
- Metacommands have, where it makes sense, shortcuts like `ifTrue' which takes either a Bool or a TValue so the user doesn't have to specify conversion functions all the time.

Currently the only downside to this approach is that GHC cannot automatically infer the right type for numeric literals. 
Since we use a ToTValue class to use types interchangably in our metacommand shortcuts we have an issue with overlapping instances with numeric literals.
The current solution to this is to annotate the literal with a type like `set "mylist" ([1..3] :: [Int])`.

In order to make sure values can be accessed in a user-friendly manner when fetching a value with get, InserVar, IfVar or SetVar we we introduce JavaScript-like notation:

- Values inside a map can be accessed with a '.': `{{ InsertVar "product.name" }}`.
- Values inside a list can be accessed with '[index]': `{{ InsertVar "products[0].name" }}`
- This works in a nested manner so `{{ InsertVar "products[0].price.total" }}` also works

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
