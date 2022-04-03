

(TODO: Add cover page support to MarkTeX)
Title: The MarkTeX document system

# Introduction

MarkTeX is a document preparation and generation system. 


In the spirit of eating our own dog food, this report has been prepared and generated using MarkTeX. The source files can be found [here](https://github.com/ProbablyFaiz/infoafp-project/tree/master/report)

# Architecture

MarkTeX consists of three main components: the eponymous MarkTeX language, a Haskell-based templating system, and a TeX backend for creating PDF documents. We describe each of these components in turn.

## The Language

The MarkTeX language is a superset of the ubiquitous Markdown language.\footnote{The most authoritative specification of Markdown is the CommonMark spec. In reality, MarkTeX does not support *all* features set out by Commonmark: some features, such as raw HTML blocks are incompatible with the tool's purpose of generating PDF documents.}

```
# A sample MarkTeX document

MarkTeX supports all the usual features of Markdown, like **bold** and
*italic formatting*, as well as [hyperlinks]
(https://www.youtube.com/watch?v=dQw4w9WgXcQ).
```


## Template System

## TeX Backend
