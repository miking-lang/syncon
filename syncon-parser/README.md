# Syncon Parser

This is a working implementation of a parser based on syncons. At present, it does not implement
any static ambiguity checking, nor does it give suggestions on how to fix parse-time ambiguities, or
even nice printing of ambiguities (though it does localize them, i.e., there may be multiple errors,
but they'll be minimal).

To run you need to have [stack](https://docs.haskellstack.org/en/stable/README/), then run:

```sh
stack setup  # Might not be needed, but eh
stack build
stack exec syncon-parser -- examples/bootstrap.syncon examples/broken.syncon out.html
```

Here `examples/bootstrap.syncon` is the definition file of the language to parse, `examples/broken.syncon` is the source code file to parse, and `out.html` is the output file. If there are errors, those will be printed, otherwise `out.html` will contain an interactive visualization of the parsed syntax tree.
