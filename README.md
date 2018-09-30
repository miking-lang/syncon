# Syncon

This is the implementation that came out of my master's thesis. If you're looking for the specific state it was in for some paper that referred to it, look at the tags (releases in GitHub).

The base idea is to define programming languages in small pieces, so called 'syncons', that can then be reused across languages. Each syncon is a singular language construct, e.g., an if-statement, an anonymous function, or a pattern.

## Building

Make sure you have [stack](https://docs.haskellstack.org/en/stable/README/) installed, then run:

```sh
stack setup
stack build
```

## Running

To run the interpreter:

```sh
stack exec implementation-exe -- File languages/core languages/lua/language languages/lua/file
```

The arguments are as follows:

- `--`: everything after here is passed to the interpreter, as opposed to `stack`.
- `File`: the syntax type expected to cover the entire file. All language implementations under `languages` are written to use `File`.
- `languages/core`: the syncons and syntax types that are part of the core language. The included interpreter (in `src/Interpreter.hs`) knows how to run this language.
- `languages/lua/language`: the language the final source file is written in. Note that you may supply multiple languages here, in which case each language may use syncons from all prior languages, while the final source file only uses the last languages definition.
- `languages/lua/file`: the source file to parse and run.

An example that uses multiple language definitions, building a tower of languages:

```sh
stack exec implementation-exe -- -iec File languages/core languages/ocaml/language languages/lua-with-match/language languages/lua-with-match/file
```

Note that the OCaml has a few syncons that do not pass the expansion checker and thus requires `-iec` (or `--ignore-expansion-checker`) to proceed past that phase (The expansion is correct as the language appears now, but it assumes that no `Pattern` will ever `#bind before`, which is currently true, but may not be in the future, so the expansion checker rejects it).
