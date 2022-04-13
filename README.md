# MarkTeX

MarkTeX contains the libraries for the template language MarkTeX.TemplateLang and for MarkTeX itself.
It also contains the executable MarkTeX and a test suite for different steps in the process of this executable.

# Usage

All the commands specified below must be run in the top-level directory of the MarkTeX repository.

You can build the project by running the following:
`stack build`

To run the program for an input Markdown/MarkTeX file *input.md*, an output file *output.pdf*:
`stack run -- input.md output.pdf`

To run it with data from a file *data.json*:
`stack run -- input.md output.pdf data.json`

Running the test suite can be done with:
`stack test`
