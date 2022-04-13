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

`TValue' is defined as **data** TValue = TString **String** | TNumber **Float** | TBool **Bool** | TList **```[TValue]```** | TData **TData** | TNull.
This closely represents the JSON datatype which is the data that we will be working with.
In order to keep the Haskell code within the template from becoming very verbose we applied a few tricks:

- TValue has instances for Ord, Eq, Num, Fractional and Real. This allows basic operators to work between TValue and TValue and between TValue and numbers.
- TValue has an IsList instance which allows list-like pattern matching. We've also overridden the (++) operator to work with both TValue and List interchangably.
- We've overridden the Boolean operators to work with both TValue and Bool interchangably. All values of TValue are mapped to boolean values, e.g. an empty List is mapped to False.
- Metacommands have, where it makes sense, shortcuts like `ifTrue' which takes either a Bool or a TValue so the user doesn't have to specify conversion functions all the time.

Currently the only downside to this approach is that GHC cannot automatically infer the right type for numeric literals. 
Since we use a ToTValue class to use types interchangably in our metacommand shortcuts we have an issue with overlapping instances with numeric literals.
The current solution to this is to annotate the literal with a type like ````set "mylist" ([1..3] :: [Int])'```.

In order to make sure values can be accessed in a user-friendly manner when fetching a value with get, InserVar, IfVar or SetVar we we introduce JavaScript-like notation:

- Values inside a map can be accessed with a '.': ```{{ InsertVar "product.name" }}```
- Values inside a list can be accessed with '[index]': ```{{ InsertVar "products[0].name" }}```
- This works in a nested manner so ```{{ InsertVar "products[0].price.total" }}``` also works
