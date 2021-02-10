---
title: Another mini expression evaluator
weight: 7
katex: true
---

# Checking for satisfiability

In this exercise, we're going to develop a _very_ simple SAT solver.
To do so, we'll first build data structures to represent boolean
expressions. An expression is
[_SAT_](https://en.wikipedia.org/wiki/Boolean_satisfiability_problem)
if there is a consistent assignment of boolean values to the variables
in the expression such that it evaluates to `True`.

{{< hint info >}}
The point of this exercise is not to develop a good SAT solver. We are
doing the naivest possible thing.

If you want an extension that walks you through a classic algorithm,
try the [2019 Haskell test](http://wp.doc.ic.ac.uk/ajf/sat/) from Tony
Field at Imperial.
{{< /hint >}}

The template file for this exercise is available as
[`code/sat-exercise6.hs`]({{< code-ref "sat-exercise6.hs" >}})

## Introduction
We'll use a data type to represent out expressions
```hs
data Expr = Const Bool
  | Var String
  | Not Expr
  | And [Expr]
  | Or [Expr]
  deriving Eq
```

We implement a custom `Show` instance to pretty-print it.

The `And` and `Not` operators are n-ary, rather than binary. So they
behave like
[`and`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#v:and)
and
[`or`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#v:or)
by accepting a list of expressions.

Our solver will be very simple, we will just find all the free
variables in an expression and exhaustively try all possible
assignments of boolean values to these variables, checking one such
assignment results in the expression evaluating to `True`.

## Evaluating an expression

Now we need a way of representing a binding of variables to boolean
values, we will use a list of 2-tuples for this, and introduce a type
to save on typing
```hs
type Bindings = [(String, Bool)]
```

{{< exercise >}} 

Your first job is to write a function which, given a variable name (of
type `Strring`), looks it up in a list of bindings and returns the
corresponding value. You can assume that the bindings will contain no
duplicate entries (we will ensure this later). So define a function

```hs
find :: String -> Bindings -> Bool
```

Next we will write a function `eval` that takes an expression, and
some variable bindings, and evaluates the expression:
```hs 
eval :: Expr -> Bindings -> Bool
```

Test this function on some expressions for which you know the
answer. For example:
```hs
Prelude> eval (Var "a") [("a", False)]
False
Prelude> eval (Or [(Var "a"), (Const True)]) [("a", False)]
True
```
{{< /exercise >}}

## Enumerating all boolean assignments

We are going to test all possible assignments until we have one that
satisfies the expression, or else we run out of options (in which case
the expression is not satisfiable). We need three things for this

{{< exercise >}}
1. A way of extracting variables from an expression
   ```hs
   vars :: Expr -> [String]
   ```
2. A way of generating all boolean assignments for these variables.
   For these we will take an integer giving the number of variables and
   return lists of boolean values of this length:
   ```hs
   bools :: Int -> [[Bool]]
   ```
3. A function which glues these pieces together and returns a list
   of bindings.
   ```hs
   bindings :: Expr -> [Bindings]
   ```
{{< /exercise >}}

The vars function should just (similar to `eval`)
destructure the expression and return the concatenation of all the
variable names it finds. You may find the function `concatMap`
useful. Do not worry about generating duplicate variable names in this
list, we will remove them separately.

For generating all boolean lists of length $n$, notice that the
following simple recursive approach works. If $n = 0$ the list is
empty. If $n > 0$, the list is obtained by separately prepending both
True and False to the lists obtained with $n - 1$.

For constructing the list of bindings we will find all the unique
variables in an expression, and zip them up with each set of boolean
assignments. To build unique variables, you may find the function
```hs
uniquify :: Eq a => [a] -> [a]
uniquify [] = []
uniquify (x:xs) = x : filter (/= x) (uniquify xs)
```
useful.


## The solver

{{< exercise >}}
Now we have all the pieces in place, we can write the SAT solver
itself. Our goal is to write the function:
```hs
sat :: Expr -> Maybe Bindings
```
{{< /exercise >}}

If the expression is satisfiable it should return the variable
assignments which result in the expression evaluating to True.
Conversely, if the expression is not satisfiable, you should return
`Nothing`.

```hs
sat' :: Expr -> [Bindings] -> Maybe Bindings
```
which iterates through the list of bindings until one of them results
in the expression being satisfied.
