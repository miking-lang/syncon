# Ambiguity

Pre-existing assumption: ambiguity in a programming language is fine as long as the compiler can present a good error message. However, this error message is useless if the programmer cannot fix the error; this work is about making sure that never happens.

## Motivating Example

Consider this list literal in OCaml:

```ocaml
[a; b]
```

Assuming an intuitive description of the syntax of OCaml we get an ambiguity: "`;`" is used both as an item separator in a list, and as sequential composition. The two trees are as follows:

```graph
digraph {
  node [shape=box];

  subgraph cluster_a {
    label = <2 element list>;
    list1 [label = "list"];
    var11 [label = "var (a)"];
    var12 [label = "var (b)"];
    list1 -> {var11 var12};
  }

  subgraph cluster_b {
    label = <Sequential composition>;
    list2 [label = "list"];
    var21 [label = "var (a)"];
    var22 [label = "var (b)"];
    list2 -> seq;
    seq -> {var21 var22};
  }
}
```

We assume that (a) the only means of disambiguation availible to the programmer is grouping (e.g. by parentheses), (b) grouping is always available, and (c) grouping will never be mistaken for something else. Adding parentheses (e.g. "`[(a; b)]`") then places a restriction on what parse trees are available: there must be a node in the parse tree that covers exactly the grouped range of code (i.e. "`a; b`"). This is because grouping has a single child node, which covers everything but the parentheses.

Adding parentheses around "`a; b`" thus forbids the left parse tree above; it has no node that covers *exactly* that range. Since we only had two trees to start with we now only have one valid parse tree, thus we are done with disambiguation.

The problem is that there is no way to forbid the right parse tree without also forbidding the left one, thus we can never select the left tree. We can see this by attaching range information to each node and computing a set of ranges covered for each parse tree:

```graph
digraph {
  node [shape=box];
  forcelabels = true;

  subgraph cluster_a {
    label = <2 element list<br/>{1:5, 2:2, 4:4}>;
    list1 [label = "list", xlabel="1:5"];
    var11 [label = "var (a)", xlabel="2:2"];
    var12 [label = "var (b)", xlabel="4:4"];
    list1 -> {var11 var12};
  }

  subgraph cluster_b {
    label = <Sequential composition<br/>{1:5, 2:2, <B>2:4</B>, 4:4}>;
    list2 [label = "list", xlabel="1:5"];
    var21 [label = "var (a)", xlabel="2:2"];
    var22 [label = "var (b)", xlabel="4:4"];
    seq [label = "seq", xlabel="2:4"];
    list2 -> seq;
    seq -> {var21 var22};
  }
}
```

The rangeset of the left tree is a subset of the right tree, grouping places a constraint that requires the presence of a range, thus the left tree can never be allowed when the right is forbidden. Note that we do want subset, as opposed to proper subset; two equal rangesets are also indistinguishable by grouping.

The goal then is to (statically) determine if a grammar can produce these unresolvable ambiguities.

## Grammars

The shape of the grammars we consider matter rather more here than for most other questions concerning languages. Transformations on grammars that preserve the language (e.g., transformation to Chomsky normal form) are likely to change the shape of the *parse trees* produced, which are important for our current question.

In particular, if each node in the parse tree is defined with EBNF (as in syncons), we cannot simplify the problem and transform that grammar to a normal CFG since that will introduce new nodes in the parse trees, each of which add new ranges to the rangeset. For example, the syncon definition of lists in OCaml would produce the following parse tree if it was transformed into a normal CFG:

```graph
digraph {
  node [shape=box];
  forcelabel=true;

  list [xlabel = "1:5"];
  "?" [xlabel = "2:4"];
  "var (a)" [xlabel = "2:2"];
  "*" [xlabel = "3:4"];
  "var (b)" [xlabel = "4:4"];

  list -> "?";
  "?" -> "var (a)";
  "?" -> "*";
  "*" -> "var (b)";
  "*" -> "\"\"";
}
```

Notably, this rangeset contains "`2:4`", which was previously only present in the sequential composition parse tree. This is solvable, e.g., by marking the important node and only considering those when computing a rangeset, but it must be taken into consideration.

## Visibly Pushdown Languages

This section simplifies slightly, to only cover what we actually need for my intended approach. The description actually describes a proper subset of visibly pushdown languages.

Visibly pushdown languages, alternatively input-driven languages or nested word languages, has (I believe^[There is a [project](https://github.com/ianh/owl) that does it, and I believe the approach is correct. I aim to recreate it to make sure]) decidable ambiguity checking. I believe that this check can be modified to decide our "unresolvably ambiguous" property. Background first, however:

A VPL uses a split alphabet, three disjoint sets, one for opening brackets ($Z_($), one for closing brackets ($Z_)$), and one for all other terminals ($Z_*$). It can then be parsed by a pushdown automata where each edge with a label in $Z_($ pushes exactly one symbol on the stack, each edge with a label in $Z_)$ pops exactly one symbol off the stack, and all other edges leave the stack unchanged.

A slightly more intuitive formulation is that a VPL is a set of named regular expressions, where the different regexes can refer to each other (including recursive uses) as long as each cycle goes through at least one guarded reference. Guarded here means "surrounded by brackets", i.e., there is a terminal in $Z_($ before the reference and a terminal in $Z_)$ after it. Additionally, these brackets must be balanced (though not necessarily matching, e.g., we could pair "(" with "]" or similarly).

## Alternate formulation of unresolvably ambiguous

Given an EBNF grammar we can produce a VPL that encodes both the word generated by the original grammar as well as its parse tree. In every production, replace each non-terminal $A$ with $(A)$, add a new starting non-terminal $S'$, and add a new production $S' \rightarrow (S)$ (where $S$ is the previous starting non-terminal).

```
Expr -> '[' (Expr (';' Expr)*)? ']'
Expr -> Expr ';' Expr
Expr -> 'a'
Expr -> 'b'

# Becomes:

Expr' -> '(' Expr ')'
Expr -> '[' ('(' Expr ')' (';' '(' Expr ')')*)? ']'
Expr -> '(' Expr ')' ';' '(' Expr ')'
Expr -> 'a'
Expr -> 'b'
```

This language puts parentheses around every node present in a parse tree of the original grammar. They thus also represent the ranges of those nodes, giving us a rangeset embedded in the string^[Technically, we get a range*bag*, duplicate ranges are allowed. This happens if a production accepts a single non-terminal on the righthand side (e.g. $N$, or a?$N$?)]. This language is clearly VPL according to the definition in the previous section. If this new language is ambiguous, that means that we have two distinct parse trees in the original grammar that both produce the same word and the same rangeset. We can thus check for *equality* between rangesets. The subset relation corresponds to seeing if we can take one string in the language, remove a bunch of (paired) parentheses, and then get a new string that is also in the language. Detecting this is now our new goal.

Slightly more formally; given:

- a thusly generated VPL, call it $V$,
- a relation $\Rightarrow$ between strings: if we can remove a pair of parentheses from a string $w$ and get a string $w'$, then $w \Rightarrow w'$, and
- the (non-reflexive) closure $\Rightarrow^+$ of $\Rightarrow$,

we wish to determine if $\exists w, w' \in V.\ w \Rightarrow^+ w'$.

First, we formulate a partial solution. Note that we can merge productions with the same left-hand side, if we allow "`|`" on the right-hand side:

```
Expr' -> '(' Expr ')'
Expr -> '[' ('(' Expr ')' (';' '(' Expr ')')*)? ']'
      | '(' Expr ')' ';' '(' Expr ')'
      | 'a'
      | 'b'
```

This gives us one regex per non-terminal, and from those we can produce one DFA^[Determinization and minimization is ok to do because we can do ambiguity detection between regexes (Brabrand et al. 2010), and then forbid that kind of ambiguity, since it will immediately produce unresolvable ambiguity] per non-terminal:

```graph
digraph {
  rankdir=LR;
  node [shape=circle, label=""];

  subgraph cluster_Exprp {
    label = "Expr'";
    Spstart [shape=point];
    Spstart -> Sp1;
    Sp1 -> Sp2 [label = "("];
    Sp2 -> Sp3 [label = "Expr"];
    Sp3 -> Sp4 [label = ")"];
    Sp4 [shape=doublecircle];
  }

  subgraph cluster_Expr {
    label = "Expr";
    Estart [shape=point];
    Estart -> 1;
    1 -> 2 [label="["];
    1 -> 3 [label="a"];
    1 -> 3 [label="b"];
    1 -> 4 [label="("];
    2 -> 3 [label="]"];
    2 -> 5 [label="("];
    3 [shape=doublecircle];
    4 -> 6 [label="Expr"];
    5 -> 7 [label="Expr"];
    6 -> 8 [label=")"];
    7 -> 9 [label=")"];
    8 -> 10 [label=";"];
    9 -> 11 [label=";"];
    9 -> 3 [label="]"];
    10 -> 12 [label="("];
    11 -> 5 [label="("];
    12 -> 13 [label="Expr"];
    13 -> 3 [label=")"];
  }
}
```

We can now parse our language by moving between the DFAs: when we are to traverse an edge with a non-terminal, move to the corresponding DFA, parse there until we reach a final state and the next terminal in the string is a closing bracket, then go back to the previous DFA and finish the transition, then proceed as normally.

We note that each of these DFAs are individually unambiguous^[A DFA is unambiguous if each accepted string has a unique path through the automaton] (by construction). Additionally, each "`(`" transition is immediately followed by a non-terminal transition, and each non-terminal transition is immediately followed by a "`)`" transition (also by construction). From the latter point we merge the three transitions and create equivalent DFAs:

```graph
digraph {
  rankdir=LR;
  node [shape=circle, label=""];

  subgraph cluster_Exprp {
    label = "Expr'";
    Spstart [shape=point];
    Spstart -> Sp1;
    Sp1 -> Sp2 [label = "(Expr)"];
    Sp2 [shape=doublecircle];
  }

  subgraph cluster_Expr {
    label = "Expr";
    Estart [shape=point];
    Estart -> 1;
    1 -> 2 [label="["];
    1 -> 3 [label="a"];
    1 -> 3 [label="b"];
    1 -> 8 [label="(Expr)"];
    2 -> 3 [label="]"];
    2 -> 9 [label="(Expr)"];
    3 [shape=doublecircle];
    8 -> 10 [label=";"];
    9 -> 11 [label=";"];
    9 -> 3 [label="]"];
    10 -> 3 [label="(Expr)"];
    11 -> 9 [label="(Expr)"];
  }
}
```

Now we create a copy of each automaton. In each copy, for each edge from state $p$ to state $q$ labelled "$(N)$" (where $N$ is a non-terminal), add an $\epsilon$-edge from $s$ to an inlined copy of the DFA for $N$ and an $\epsilon$-edge from that copy to $q$:

```{.graph caption="(Note that the final states of the inlined copies are no longer final, they're just drawn that way for illustrative purposes)"}
digraph {
  rankdir=LR;
  node [shape=circle, label=""];

  subgraph cluster_Exprp {
    label = "Expr' Copy";
    Spstart [shape=point];
    Spstart -> Sp1;
    Sp1 -> Sp2 [label = "(Expr)"];
    subgraph cluster_Exprp_ExprC1 {
      label = "Expr Copy";
      Exprp_ExprC1s -> Exprp_ExprC1e;
      Exprp_ExprC1e [shape=doublecircle];
    }
    Sp1 -> Exprp_ExprC1s;
    Exprp_ExprC1e -> Sp2;
    Sp2 [shape=doublecircle];
  }

  subgraph cluster_Expr {
    label = "Expr";
    Estart [shape=point];
    Estart -> 1;
    1 -> 2 [label="["];
    1 -> 3 [label="a"];
    1 -> 3 [label="b"];
    1 -> 8 [label="(Expr)"];
    subgraph cluster_ExprC1 {
      label = "Expr Copy";
      ExprC1s -> ExprC1e;
      ExprC1e [shape=doublecircle];
    }
    1 -> ExprC1s;
    ExprC1e -> 8;
    2 -> 3 [label="]"];
    2 -> 9 [label="(Expr)"];
    subgraph cluster_ExprC2 {
      label = "Expr Copy";
      ExprC2s -> ExprC2e;
      ExprC2e [shape=doublecircle];
    }
    2 -> ExprC2s;
    ExprC2e -> 9;
    3 [shape=doublecircle];
    8 -> 10 [label=";"];
    9 -> 11 [label=";"];
    9 -> 3 [label="]"];
    10 -> 3 [label="(Expr)"];
    subgraph cluster_ExprC3 {
      label = "Expr Copy";
      ExprC3s -> ExprC3e;
      ExprC3e [shape=doublecircle];
    }
    10 -> ExprC3s;
    ExprC3e -> 3;
    11 -> 9 [label="(Expr)"];
    subgraph cluster_ExprC4 {
      label = "Expr Copy";
      ExprC4s -> ExprC4e;
      ExprC4e [shape=doublecircle];
    }
    11 -> ExprC4s;
    ExprC4e -> 9;
  }
}
```

Pardon the messy graphs. Each inlined copy gets fresh states. Note that the copy is now likely an $\epsilon$-NFA rather than a DFA. The next part I have difficulties explaining in an understandable way. If you have difficulties, move on to the next section for an alternate explanation.

We now look for one path in the DFA, and one in the $\epsilon$-NFA, both producing the same string, but the latter path includes at least one state from an inlined copy. The latter path corresponds to parsing a string in the VPL, but skipping one (or more) pairs of parentheses^[Important caveat: skipping one or more *outermost* pairs of parentheses]. The former corresponds to parsing a string in the VPL without skipping parentheses. If we find two such paths, and they produce the same string, they correspond to two strings in $w, w' \in V$ where $w \Rightarrow w'$ (the path in the DFA corresponds to $w'$, the path in the $\epsilon$-NFA corresponds to $w$).

## Alternate line of reasoning, thinking more about trees directly

From before, we have that each level of a parse tree we consider is described by a regular expression, e.g.:

```graph
graph {
  rankdir=LR;
  node [shape = box];
  subgraph cluster_Expr1 {
    label = "Expr (1 element list)";
    1 [label = "["];
    3 [label = "]"];
    1 -- Expr3 -- 3 [style=invisible];
    Expr3 [label = "Expr"];
  }
  subgraph cluster_Expr2 {
    label = "Expr (2 element list)";
    "[" -- Expr1 -- ";" -- Expr2 -- "]" [style=invisible];
    Expr1 [label = "Expr"];
    Expr2 [label = "Expr"];
  }
}
```

If we look at two levels for the 1 element list tree, e.g.:

```graph
digraph {
  rankdir=LR;
  compound = true;
  node [shape = box];
  subgraph cluster_Expr1 {
    label = "Expr (1 element list)";
    1 [label = "["];
    3 [label = "]"];
    1 -> Expr3 -> 3 [style=invisible, arrowhead=none];
    Expr3 [label = "Expr"];
  }
  subgraph cluster_Expr2 {
    label = "Expr";
    2 [label=";"];
    Expr1 [label="Expr"];
    Expr2 [label="Expr"];
    Expr1 -> 2 -> Expr2 [style=invisible, arrowhead=none];
  }
  Expr3 -> 2 [lhead=cluster_Expr2];
}
```

Inlining the child into the parent yields:

```graph
graph {
  rankdir=LR;
  node [shape = box];
  subgraph cluster_Expr2 {
    label = "Expr (1 element list with sequential composition inlined)";
    "[" -- Expr1 -- ";" -- Expr2 -- "]" [style=invisible];
    Expr1 [label = "Expr"];
    Expr2 [label = "Expr"];
  }
}
```

This (partial tree) is identical to the 2 element list one. This inlining operation is exactly equivalent with $\Rightarrow$ from the previous section, we are thus looking for two trees, potentially partial, where one can be obtained by inlining some number of times in the latter.

Thoughts in progress:

- might be worth properly formulating the property in the form of these trees
- The trees themselves can be described by a regular tree language, might be possible to formulate the search in terms of tree automata in some way.

# References
Brabrand, Claus, and Jakob G. Thomsen. "Typed and Unambiguous Pattern Matching on Strings Using Regular Expressions." In Proceedings of the 12th International ACM SIGPLAN Symposium on Principles and Practice of Declarative Programming, 243–254. PPDP ’10. New York, NY, USA: ACM, 2010. https://doi.org/10.1145/1836089.1836120.
