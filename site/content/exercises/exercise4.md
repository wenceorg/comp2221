---
title: Lazy evaluation and infinite datastructures
weight: 4
katex: true
---

# Lazy data, infinite data

Haskell does not evaluate expressions eagerly, but instead does so
_lazily_. This opens the door to writing very succint programs that
operator on infinite datastructures. We can just write them without
needing to have a cut-off and then pick out the pieces we want.

For some more examples, on top of the ones we have here, there's a
nice
[Computerphile](https://www.youtube.com/channel/UC9-y-6csu5WGm29I7JiwpnA)
video on infinite datastructures, [featuring Graham
Hutton](https://www.youtube.com/watch?v=bnRNiE_OVWA). There's a nice
succint post on some of Haskell's advantages [including
laziness](https://serokell.io/blog/10-reasons-to-use-haskell#laziness)
from [serokell](https://serokell.io).

On with the show.

## Simple sieving

Our first go with infinite data is going to be to generate prime
numbers. Let's first generate all the integers greater than or equal
to 2.

```hs
candidates :: [Integer]
candidates = [2..]
```
These are our candidate primes. We can now generate prime numbers
using the [sieve of
Eratosthenes](https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes).
The first element of our candidates is a prime number, we then need to
cross off all multiples, and repeat.

{{< exercise >}}

Write a function

```hs
sieve :: [Integer] -> [Integer]
```

that generates all prime numbers.

{{< details Hint >}}
Think about using a list comprehension to remove values from your
candidate list that are multiples of the current prime.
{{< /details >}}
{{< /exercise >}}

{{< hint info >}}

You'll probably not want to print this list, since [Euclid
showed](https://en.wikipedia.org/wiki/Euclid%27s_theorem) that it is
infinite in length a while ago.

Instead, to look at (say) the first five elements use

```hs
take 5 (sieve candidates)
```
{{< /hint >}}

{{< question >}}

Did you come up with a one-liner using `mod` in your answer? If so, you probably ended up implementing
finding primes by [trial
division](https://en.wikipedia.org/wiki/Trial_division).

The prime sieve shouldn't need to do any arithmetic, doing this
properly is actually slightly fiddly, if you're interested, this [nice
paper](https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf) explains
the problem with the one-liner approach.

Don't worry, you're not alone in doing this, if you watched the video
with Graham Hutton above, you might have spotted that he does the same
thing.
{{< /question >}}

## Pythagorean triples again

[Last time]({{< ref "exercise3.md" >}}) we implemented code to
generate all Pythagorean triples with entries less than some integer
$n$.

This time round, we're going to do something a bit smarter and
generate an _infinite_ list of all [_primitive_ Pythagorean
triples](https://en.wikipedia.org/wiki/Tree_of_primitive_Pythagorean_triples).
Recall that a Pythagorean triple is a tuple of three positive integers
$(a, b, c)$ such that
$$
a^2 + b^2 = c^2,
$$
it is called _primitive_ if $a$, $b$, and $c$ are all coprime.

All such primitive triples can be generated from the root truple $(3,
4, 5)$ by using [matrix
generators](https://en.wikipedia.org/wiki/Tree_of_primitive_Pythagorean_triples).

In particular, consider $m > n > 0$ with $m$ and $n$ coprime (and one
even and the other odd). Then
{{< rawhtml >}}
<div id="eq1">
$$
\begin{aligned}
 a &= m^2 - n^2 \\
 b &= 2 m n\\
 c &= m^2 + n^2
\end{aligned}
$$
</div>
{{< /rawhtml >}}

is a new primitive Pythagorean triple. Writing $\vec{M} = [m, n]$ as a
column vector, then the required properties are preserved by
premultiplying $\vec{M}$ by any of the three matrices

{{< rawhtml >}}
$$
\begin{bmatrix}
2 & -1\\
1 & 0
\end{bmatrix},
\begin{bmatrix}
2 & 1 \\
1 & 0 \\
\end{bmatrix},\text{ or }
\begin{bmatrix}
1 & 2 \\
0 & 1
\end{bmatrix}.
$$
{{< /rawhtml >}}

We will use these to generate a tree of all Pythagorean triples. We'll
build this up in stages by implementing some helper functions.

{{< exercise >}}

First write a function
```hs
mult :: [[Int]] -> [Int] -> [Int]
```
which computes the result of multiplying a matrix (represented as a
list of lists) onto a vector (represented as a list). You may find the
functions `zipWith` and
`repeat` helpful.

Recall that for a matrix, $A \in \mathbb{R}^{n \times m}$ and vector
$x \in \mathbb{R}^{m}$, the product $\mathbb{R}^n \ni y = Ax$ is
defined as
$$
y_i = \sum_{j=1}^{m} A_{ij} x_j,
$$
where $A_{ij}$ is the entry in the $i^{\text{th}}$ row
and $j^{th}$ column of $A$.
    
{{< /exercise >}}

Next up, we need to convert an $(m, n)$ pair into a triple

{{< exercise >}}
Write 
```hs
pythTriple :: [Int] -> (Int, Int, Int)
```
which uses the relationship [above]({{< ref "exercise4.md#eq1" >}}) to generate a triple from a 2-element
list. For example
```
Prelude> pythTriple [2, 1]
(3, 4, 5)
Prelude> pythTriple [3, 2]
(5, 12, 13)
```
{{< /exercise >}}

Finally, this slightly trickier part, generating the tree of all these
$(m, n)$ pairs.

{{< exercise >}}

Write a function

```hs
treeGen :: [[Int]] -> [[Int]]
```

This can be written by using the three generator matrices to create a
list of the next tree pairs, and then appending the result of a
recursive call to `treeGen` to the list.

You may find one of `concatMap` or `concat` useful

```hs
concatMap :: (a -> [b]) -> [a] -> [b]
concat :: [[a]] -> [a]
```

{{< details Hint >}}
Hint: be careful that you don't recurse depth-first down one
of the leaves of the tree (as I did the first go I had)
{{< /details >}}

For example, you should see something like
```
Prelude> take 10 (treeGen [[2, 1]])
[[3,2],[5,2],[4,1],[4,3],[8,3],[7,2],[8,5],[12,5],[9,2],[7,4]]
```
Although the order may be different.

Finally, we can create the pythagorean truples by mapping `pythTriple`
over the list of pairs obtained from `treeGen`, starting with `[2,
1]`.

{{< /exercise >}}

{{< solution release=true >}}
## Solutions

I've added some [commented solutions]({{< code-ref
"exercises/exercise4-solutions.hs" >}}) to these exercises. If you
have queries about them please ask in the practical sessions or else
[get in touch]({{< ref "/#discussion-forum" >}}).

{{< /solution >}}
