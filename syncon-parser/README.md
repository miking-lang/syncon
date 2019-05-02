# Syncon Parser

This is a working implementation of a parser based on syncons. At present, it does not implement
any static ambiguity checking, nor does it give suggestions on how to fix parse-time ambiguities, or
even nice printing of ambiguities (though it does localize them, i.e., there may be multiple errors,
but they'll be minimal).

To run you need to have [stack](https://docs.haskellstack.org/en/stable/README/) installed, then run:

```sh
cd syncon-parser # Ensure you are in the correct directory
stack setup      # Might not be needed, but eh
stack build
stack exec syncon-parser -- examples/bootstrap.syncon examples/broken.syncon out.html
```

Here `examples/bootstrap.syncon` is the definition file of the language to parse, `examples/broken.syncon` is the source code file to parse, and `out.html` is the output file. If there are errors, those will be printed, otherwise `out.html` will contain an interactive visualization of the parsed syntax tree.


## Basics of Defining the Syntactical Aspects of a Language

At present, a language definition is a single file containing various definitions. This section is a brief description of how those definitions are written. More examples can be found in the `examples` directory.

------

A syntax type is declared as follows:

```
type Top
type Exp
type Pattern
```

`Top` in particular is required; a source code file will be expected to consist of zero or more things with syntax type `Top`.

------

A token syntax type, i.e., a syntax type that matches a single terminal, is defined as follows:

```
token Ident = "[[:lower:]][[:word:]]*"
token Integer = "[0-9]+"
token String = "\"(\\\\.|[^\"\\\\])*\""
```

The string on the right hand side is a (PCRE) regex.

------

A comment, i.e., a terminal that is ignored, is defined as follows:

```
comment "//[^\\n]*(\\n|$)"
```

------

A syncon is defined as follows:

```
syncon topLetRec: Top =
  "let" "rec" x:Ident args:Pat*
  "=" e:Exp
  more:("and" x2:Ident args2:Pat*
        "=" e2:Exp)* ";;"?
{ builtin }
```

The right hand side, between `=` and `{`, is the syntax description, which describes the syntactical form of a syncon. It is similar to a regular expression, but does not allow choice (i.e. `|`), and allows naming things. The named things will later be part of the abstract syntax tree that is constructed, so put a name on everything that shouldn't be thrown away.

The body (between `{` and `}`) is unspecified at the moment, since this project only considers parsing for now. Thus every syncon must be declared as `builtin`.

As a convenience, if the body is merely `{ builtin }`, it may be omitted, like so:

```
syncon topLetRec: Top =
  "let" "rec" x:Ident args:Pat*
  "=" e:Exp
  more:("and" x2:Ident args2:Pat*
        "=" e2:Exp)* ";;"?
```

------

An operator is defined similarly, but using `infix`, `prefix`, or `postfix` instead of `syncon`. For example:

```
infix sum: Exp = "+"
prefix not: Exp = "!"
postfix question: Exp = "?"
```

These are more or less syntax sugar for :

```
syncon sum: Exp = left:Exp "+" right:Exp
syncon not: Exp = "!" right:Exp
syncon question: Exp = left:Exp "?"
```

`infix` in particular can have associativity, specified by adding `left` or `right` before the name:

```
syncon left prod: Exp = "*"
syncon right funcType: Type = "->"
```

------

Precedence between operators can be declared using a precedence list:

```
precedence {
  product fraction;
  sum difference;
  equal leq lt;
} except {
  equal leq lt;
}
```

Operators appearing on the same line have the same precedence and those appearing higher (i.e., earlier lines) have higher precedence. The exception list at the end specifies that this list should *not* define the precedence of certain operators, in this case, none of `equal`, `leq`, and `lt` have defined precedence relative each other.

Note that precedence is not transitive, no precedences are defined implicitly.

------

You can also specify that a given syncon cannot be the direct child of another with a `forbid` declaration:

```
syncon list: Exp =
  "[" (head: Exp (";" tail: Exp)*)? "]"

infix right seqComp:Exp = ";"

forbid list.head = seqComp
forbid list.tail = seqComp
```

------

Finally, if you have something that doesn't quite look like an operator, but you still want to take advantage of precedence, you can use `rec` as a special syntax type in the syntax description. This behaves mostly the same as just using the same syntax type literally, but interacts slightly differently with precedence lists. For example,

```
syncon ocamlTuple: Exp = eh:rec ("," et:rec)+
```

is basically the same as

```
syncon ocamlTuple: Exp = eh:Exp ("," et:Exp)+
```

except that `rec` interacts with precedence lists. Putting the former syncon in a precedence list is ok and generates forbids for each occurence of `rec`, while the latter would be an error, since it's not an operator.
