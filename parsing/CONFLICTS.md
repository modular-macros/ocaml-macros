# Conflicts

Some of the conflicts and issues in the grammar are documented here.

## A variant type that lists a single atomic type

Why can't `[t]` be considered a valid atomic type? (A variant type.)

(This is related to MPR #3835.)

A class type that begins with `[t] foo` could continue as follows:

```
  [t] foo -> <class_type>
```

Here `t` is understood as a variant type,
and is used as an actual parameter of the parameterized type `'a foo`.

Or it could continue as follows:

```
  [t] foo
```

Here `t` is a type (there is no variant type)
and is used as an actual parameter of the class `['a] foo`.

After we have read the closing bracket and are looking ahead at `foo`,
we need to decide which of the above two situations we have. (The first
situation requires a reduction; the second situation requires shifting.)
But we cannot decide yet; we would need to look at the arrow `->` beyond
`foo` in order to decide. In this example LR(2) is required; in general,
`foo` could be replaced with an arbitrary qualified name, so unbounded
lookahead is required.

As a result of this issue, we must abandon the idea that `[t]` could be
a well-formed variant type. In the syntax of atomic types, instead of:

```
  atomic_type: LBRACKET row_field RBRACKET
```

we must use the more restricted form:

```
  atomic_type: LBRACKET tag_field RBRACKET
```

In other words, we rule out exactly the following:

```
  atomic_type: LBRACKET atomic_type RBRACKET
```

## A template functor bracket must not follow an object type directly

Template functors delimit their parameter and argument with square
brackets (`module F[X : S] = ME`, `F[M]`, `[X : A] B`), which the
lexer reads as the ordinary `LBRACKET` and `RBRACKET` tokens.
Brackets are not operator characters, so two that meet never fuse --
`F[]`, `F[M][N]`, `G[F[X]]` and `[[A] B] C` all lex as intended --
with one exception: the lexer reads `>]` as a single token
(`GREATERRBRACKET`, retained from camlp4's stream-parser syntax; no
grammar rule uses it).  A parameter or argument whose module type
ends in an object type must therefore keep a space before the
closing bracket:

```
  module type T = [X : S with type t = < m : int > ] B    (* not ">]" *)
```

The failure is a syntax error, never a misparse, and `Pprintast` pads
the closing delimiter of object types, so printed output parses back.

Note also that the `functor` keyword introduces template functor
*expressions* (`functor [X : S] -> ME`) but not template functor
*types*: the type is written `[X : A] B`, with no keyword and no arrow.

## The operators `<<`, `>>` and `$` are no longer definable

MacoCaml claims three symbol sequences as tokens: `<<` and `>>`
delimit quotations, and `$` marks splices.  Consequently exactly these
three infix operators are lost — they can be neither defined nor used:

```
  let ( << ) f g x = f (g x)       (* syntax error in MacoCaml *)
  let ( >> ) f g x = g (f x)       (* syntax error in MacoCaml *)
  let ( $ )  f x   = f x           (* syntax error in MacoCaml *)
```

This is a deliberate loss, and it is limited to the exact three
sequences: longer operators sharing the prefix (`<<<`, `$$`, `$:`,
...) and the `>.`-family lex as before, because the lexer prefers the
longest match.

The longest match also applies to the closing `>>` of a quotation, so
a symbol character that follows it directly fuses with it: `<< x >>= y`
lexes `>>=` as one operator and the quotation is never closed (a syntax
error).  Separate the closer from what follows: `<< x >> = y`.

Note that a *use* of the lost operators in upstream code is not always
a syntax error: the tokens are re-interpreted, so an expression such
as `f << g >> h` — composition upstream — parses in MacoCaml as the
application `f (<< g >>) h` of `f` to a quotation, and the failure (if
any) surfaces later, as a type error.  Code being ported that defines
these operators must rename them.
