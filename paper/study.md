# Things we can do that others cannot

- Provide good error messages when things at the same precedence but weird associativity are combined.
- Define languages separately, with total precedences, that can then be used together *without* having defined precedence between operators from different languages.
  - The more "normal" corollary to this is defining different libraries with new operators, and not having operators from different libraries have defined relative precedence implicitly. This is *really* prevalent in Haskell, because of the design of their custom operators, but hardly written about. I believe there's some opinions here from beginners, but more experienced programmers knows that "this is the way it is" and don't really question it, since it becomes a much harder problem to solve then.
- Explicitly don't define precedence for operators where it's not obvious what the precedence should be, or where programmers frequently are wrong (e.g., `&&` and `||`).
- Perform similar feats for arbitrary CFG constructions, not just operators with precedence and associativity.

----------------

# typecast relative to binary operators

Turns out typecasting has higher precedence than all binary operators in the C derived languages, so putting the typecast first will do what you want.

I.e., `(double) a / b` is the same as `((double) a) / b`.

Making this ambiguous will be clearer for the user, in that it'll have to be explicit, but it won't help with fixing any of the bugs we talked about.

# bitwise operators (Bonus with `&&` and `||`)

`&`, `|`, and `^` have lower precedence than expected, leading to oddities with `1 & 3 == 1` for example.

Slips through to typechecking in languages that have separate boolean types and static types (unless the operands are boolean), otherwise goes to either a warning, or all the way to runtime.

## C Compilers

GCC has a warning `-Wparentheses` that warns "[...] if parentheses are omitted in certain contexts, such as when there is an assignment in a context where a truth value is expected, or when operators are nested whose precedence people often get confused about. [...]" https://gcc.gnu.org/onlinedocs/gcc-9.2.0/gcc.pdf

IBM Knowledge Center has a more detailed description of this, including ambiguous nested if-then-else, and nesting of `&&` and `||` without parens. https://www.ibm.com/support/knowledgecenter/en/SSB23S_1.1.0.14/common/m1rhparn.html

## Stack overflow

### Operator precedence (bitwise '&' lower than '==')
Asking about why the precedence is as it is.
5k views, 39 upvotes, answer upvotes (51, 16, 4)
https://stackoverflow.com/questions/4685072/operator-precedence-bitwise-lower-than

### GCC: suggest parentheses around comparison parentheses not able to solve myself
GCC has a warning (-Wparentheses) that suggests using parens in an expression of the form `_ & _ == _`, but doesn't give a concrete suggestion
12k views, 1 upvote, answer upvotes (6, 1, 1, 0)
https://stackoverflow.com/questions/21919922/gcc-suggest-parentheses-around-comparison-parentheses-not-able-to-solve-myself?noredirect=1&lq=1

# Nested match in Ocaml
Most commonly slips through to typechecking, but sometimes to exhaustiveness checking (TODO: produce errors for this)

## Stack overflow

### OCaml: Match expression inside another one?
The person asking does identify what the problem is (i.e., they manage to interpret the type error), but don't know how to fix it. Answer uses parens, as we would suggest.
27k views, 49 upvotes, answer upvotes (73, 9)
https://stackoverflow.com/questions/257605/ocaml-match-expression-inside-another-one

### How to handle nested match expressions
Also correctly deduces what's going on, though no type error here, and does not know how to fix it.
116 views, 0 upvotes, answer upvotes (0)
https://stackoverflow.com/questions/54156469/how-to-handle-nested-match-expressions

### OCaml nested patterns - exception despite defining pattern
Does not figure out what's going on, error message states that match isn't exhaustive.
202 views, 3 upvotes, answer upvotes (8)
https://stackoverflow.com/questions/8945213/ocaml-nested-patterns-exception-despite-defining-pattern

# User-defined operators (Haskell)

There are two distinct points here:
- Same precedence, different or no associativity gives an error that should be an ambiguity error, but is (for `1 >> 2 >>> 3`):
  ```
  Precedence parsing error
      cannot mix ‘>>’ [infixl 1] and ‘>>>’ [infixr 1] in the same infix expression
  ```
  which doesn't give any suggestion on how to fix it. This one can be searched for, but most people running into it seem to be able to solve the issue themselves, with the exception of some more complicated expressions.
- Operators from different libraries have a defined relative precedence, but most likely shouldn't. This one is really hard to search for, as it's essentially a design question, and it's non-obvious that it's a possible answer (since essentially no previous system allows undefined relative precedence / non-transitive precedence)

## Haskell wiki
On syntactic sugar, talking about precedences, that "rules like 'multiplication and division precede addition and subtraction' would be more natural. However, the Show class would no longer be so simple." This is essentially the pretty-printing problem for resolvably ambiguous languages.
https://wiki.haskell.org/Syntactic_sugar/Cons#Precedences

## Stack overflow

### Why do (!!) and (.) share precedence 9? [closed]
One of the answers contains the reasoning that "Because they deal with completely different things (function composition vs list indexing), and they were in completely different files, it's highly likely that they were not cross coordinated." This is more or less our argument that these two shouldn't have a defined relative precedence.
https://stackoverflow.com/questions/40964583/why-do-and-share-precedence-9

### Searching for precedence questions for Haskell
500 results
https://stackoverflow.com/search?q=precedence+%5Bhaskell%5D

### Syntax error with “infixl” and “infixr” operators
Writing an operator section where the operator being sectioned has higher precedence than some other operator that is not being sectioned. The error message isn't particularly helpful, but probably more shows a deficiency of operator sectioning more complicated expressions than something that is truly ambiguous.
867 views, 3 upvotes, answer upvotes (4, 2)
https://stackoverflow.com/questions/26663568/syntax-error-with-infixl-and-infixr-operators

### Why it is impossible to multiply negative numbers in haskell without brackets
An oddity in Haskell's grammar, stemming from operator sections and `-` being "both" unary and binary. It is presented with the "ambiguous infix expression" error message, but isn't actually that. The fix is present already in the question.
5k views, 8 upvotes, answer upvotes (12, 5, 3, 1, 0, -1)
https://stackoverflow.com/questions/26073878/why-it-is-impossible-to-multiply-negative-numbers-in-haskell-without-brackets

# Semicolons in JavaScript
For reference:
```javascript
function foo() {
  return
  4
}
foo() // returns undefined
// it's thus equivalent with:
// function foo() {
//   return;
//   4;
// }

function bar() {
  (4)
  (5)
}
bar() // error: 4 is not a function
// it's thus equivalent with:
// function bar() {
//   (4)(5);
// }
// and
// function bar() {
//   4(5);
// }

```

We cannot solve this with our current system, since this requires line sensitivity sometimes, but only sometimes. As far as I can tell, the reasons why these two behave differently are highly grammar-technical.

## Stack Overflow

### Why use semicolon? [duplicate]
Lot's of heated discussion...
14k views, 36 upvotes, answer upvotes (34, 17, 13, 11, 10)
https://stackoverflow.com/questions/2399935/why-use-semicolon

### Do you recommend using semicolons after every statement in JavaScript?
Locked. Naturally opinionated answers.
164k views, 52 upvotes, (74, 44, 16, 11, 10, 8, 7, 6, 5, 1, 1)
https://stackoverflow.com/questions/444080/do-you-recommend-using-semicolons-after-every-statement-in-javascript

### Are semicolons needed after an object literal assignment in JavaScript?
Answer gives example with the following line starting with `(`. Mostly recommendations to use semicolons.
10k views, 30 upvotes, answer upvotes (36, 13, 8, 3, 0, 0, 0)
https://stackoverflow.com/questions/42247/are-semicolons-needed-after-an-object-literal-assignment-in-javascript

### Should I use semicolons in JavaScript? [duplicate]
8k views, 25 upvotes, answer upvotes (31, 8, 7, 7, 6, 4, 3, 2)
https://stackoverflow.com/questions/537632/should-i-use-semicolons-in-javascript

### What rules must I follow to write valid Javascript without semicolons?
Only actually good question on this question, in my personal opinion. The conclusion, as far as I can tell, is that lines starting with `(`, `[`, `+`, `-`, or `/`, except when that leading character is part of `--`, `++`, or `//` are the problematic lines, they must have a semicolon before them.
389 views, 3 upvotes, answer upvotes (8)
https://stackoverflow.com/questions/25088708/what-rules-must-i-follow-to-write-valid-javascript-without-semicolons
