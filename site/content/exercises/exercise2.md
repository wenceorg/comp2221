---
title: Types and lists
weight: 2
---

# Thinking about types

You've probably noticed by now that GHC will complain if you write
some code where the types don't match. To do this, it uses _type
inference_ to determine the valid types of any functions you write,
and checks that everything works. For example, suppose I have a
function

```hs
allTrue :: [Bool] -> Bool
allTrue [] = True
allTrue (x:xs) = x && allTrue xs
```
which checks if every entry of a list of `Bool`s is True.

{{< hint info >}}

This function already exists and is actually called `and` in the
standard Prelude.

{{< /hint >}}

Now suppose I try and apply this function to a list of integers

```
Prelude> allTrue [1, 2, 3]

<interactive>:7:10: error:
    • No instance for (Num Bool) arising from the literal ‘1’
    • In the expression: 1
      In the first argument of ‘allTrue’, namely ‘[1, 2, 3]’
      In the expression: allTrue [1, 2, 3]
```

Here Haskell has determined that the types are wrong. It is telling me
that the deduced type of the argument, which is `Num a => [a]` (that
is, a list of numbers), does not satisfy the requirement that `a` be
of type `Bool`.

{{< exercise >}}

Try this out for yourself.

{{< /exercise >}}

Reasoning about the types of functions is a core part of writing
Haskell. Although GHC can usually infer the type of functions, there
are some circumstances where it can need a bit of help. Moreover,
type annotations provide useful documentation to the reader of the
code. Rather than having to read the body of a function to determine
that it takes a list of pairs of `Bool` and `Int` (say), we can just
see it in the type definition.

As such, you should _always_ annotate your function definitions with
their types. GHC will of course check it for you, and complain if your
annotation is not valid.

Here are some definitions without types

{{< code-include "type-snippets.hs" "hs" >}}

{{< exercise >}}
Download the file and annotate the declarations with their types.

Load it in GHC to check if you got everything right.
{{< /exercise >}}

{{< hint info >}}
As well as downloading individual files, you can always clone the
entire [course repository]({{< repo >}}), at which point the code will
live in the `code/` subdirectory.
{{< /hint >}}
