---
title: Annotated slides
draft: false
weight: 3
BookToc: false
---

# Lecture slides and video links

As the course progresses, I'll upload lecture slides, the live code
examples, and add links to the videos (accessible with a Durham
account) here.

- 2021-01-12: [Annotated slides]({{< static-ref
  "slides/2020-21/Lec01.pdf" >}}),
  [video](https://durham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=c27a685e-8450-417b-8eda-acae00dc4ebc),
  [code]({{< code-ref "lectures/Lec01.hs" >}})

  We got about halfway through the slides, we'll pick up where we left
  off next time.

- 2021-01-14: [Annotated slides]({{< static-ref
  "slides/2020-21/Lec02.pdf" >}}),
  [video](https://durham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=add4bf82-312c-4e35-a99e-acb000d97c33),
  [code]({{< code-ref "lectures/Lec02.hs" >}})

  We went through the remainder of the slides from Tuesday. I skipped
  over the dark blue slides introducing Haskell syntax since we saw
  that in the coding examples as well. We covered most of the stuff in
  the second set of slides. I'll go back over the currying business
  next time. We tried to emphasise the importance of types and showed
  that Haskell is quite strict. For a fun take on the importance of
  type safety, spend five minutes watching [Gary
  Bernhardt's](https://www.destroyallsoftware.com/) [WAT
  talk](https://www.destroyallsoftware.com/talks/wat).

  Some of you may have noticed that my definition of `xor` was not
  particularly succint. I guess I didn't manage it in live conditions!
  In Python we could have written

  ```python
  def xor(a, b):
      return a != b
  ```

  In Haskell, `!=` is written as `/=`, and I could have written

  ```hs
  xor :: Bool -> Bool -> Bool
  xor x y = x /= y
  ```

  Except that I had defined a new data type for `Bool` (for expository
  purposes) and we haven't defined equality on it yet.

- 2021-01-19: [Annotated slides]({{< static-ref
  "slides/2020-21/Lec03.pdf" >}}),
  [video](https://durham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=8e9814b4-f7d2-47d0-b966-acb500d971b0),
  [code]({{< code-ref "lectures/Lec03.hs" >}})

  We started introducing the concept of functions that might fail, and
  the Maybe datatype. We looked a little bit at polymorphism, and I
  touched on constraining polymorphic functions (more next time!),
  when we considered
  ```hs
  chopPrefix :: Eq a => [a] -> [a] -> Maybe [a]
  ```
  Much more to come!

- 2021-01-21: [Annotated slides]({{< static-ref
  "slides/2020-21/Lec03a.pdf" >}}),
  [video](https://durham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=7e061c40-b269-4879-ba02-acb700d9784d),
  [code]({{< code-ref "lectures/Lec04.hs" >}})

  We revisited the `chopPrefix` example and looked at a little bit of
  theory for different types of polymorphism. The [wikipedia
  page](https://en.wikipedia.org/wiki/Polymorphism_(computer_science))
  has a nice overview as usual. The classic work on subtyping is from
  [Barbara Liskov](https://en.wikipedia.org/wiki/Barbara_Liskov)
  from 1987. The method that Haskell uses for constrained (ad-hoc)
  polymorphism based on type classes was introduced by [Phil Wadler](http://homepages.inf.ed.ac.uk/wadler/)
  and [Stephen Blott](https://www.computing.dcu.ie/~sblott/) in [_How
  to make ad-hoc polymorphism less ad
  hoc_](http://homepages.inf.ed.ac.uk/wadler/topics/type-classes.html#class).
  [Here's a video](https://www.youtube.com/watch?v=6COvD8oynmI) of
  Simon Peyton Jones giving an introductory talk on type classes and
  their implementation in GHC.

  We introduced some example type classes and how we implement them
  for our own types.

  The "Lecture 4" slides are unannotated (so what is on DUO is fine)
  but they are also available [here]({{< static-ref
  "slides/2020-21/Lec04.pdf" >}}).

- 2021-01-26: [Annotated slides]({{< static-ref
  "slides/2020-21/Lec05.pdf" >}}),
  [video](https://durham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=5bd9d66c-b4ba-4d86-8603-acbc00d86fe5),
  [code]({{< code-ref "lectures/Lec05.hs" >}})

  We finished off writing some functions on our own list type, and
  saw two ways of writing reversal of a list, one slow and one fast.
  To understand the fast approach, I talked a little bit about tail
  recursion, and how you can write "loopy" code in a recursive
  language.

  I then discussed a step-by-step approach to writing recursive
  functions. We then got a bit side-tracked talking about what it
  means to drop a negative number of entries from a list, and I
  showed a way to enforce that dropping values from a list can only
  take positive integers. This way the type system enforces
  correctness. It's rather ugly to do this, so I added a few notes
  and pointers to what people are doing around Haskell with
  [dependent](https://serokell.io/blog/why-dependent-haskell)
  and [refinement
  types](https://ucsd-progsys.github.io/liquidhaskell-blog/), which
  provide more sophisticated approaches to type safety.

  We didn't make it as far as the "maps and folds" section of the
  slides so we'll do that next time.

  I added some commented solutions for the first two exercise pages.

- 2021-01-28: [Annotated slides]({{< static-ref
  "slides/2020-21/Lec06.pdf" >}}),
  [video](https://durham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=ec066488-74e4-4dfd-b26b-acbe00d9dd09),
  [code]({{< code-ref "lectures/Lec06.hs" >}})

  I didn't go through a lot of slides this time and mostly did the
  code (which is annotated). We looked at some list comprehensions,
  which are very similar to those available in Python.

  I also showed "parallel" list comprehensions which run multiple
  generators in parallel.

  We then talked a bit about higher order functions (of which `map` is
  an example), and I said a little bit about why these patterns are
  useful in library design. They give us a common interface with a
  contract behind which we can hide all sorts of fancy implementation.
  For example, mapping over a list can be trivially parallelised in a
  language where there is a guarantee that states are not modified in
  place (because we can do things in any order). This is the
  parallelisation paradigm that is the foundation of the
  [mapreduce](https://en.wikipedia.org/wiki/MapReduce) programming
  model.

  Finally we started looking for recognisable patterns in some of the
  recursive functions we've been writing on lists. We wrote the
  implementation of a few "summarisation" functions that linearly
  recurse on the list and combine the entries in some way to produce a
  result.

  We then spotted a common pattern and wrote a "fold" function that
  abstracted this out. We'll start there next time.

- 2021-02-02: [Annotated slides]({{< static-ref
  "slides/2020-21/Lec06a.pdf" >}}),
  [video](https://durham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=e0ef9d4e-77e9-4fb7-91cd-acc300dc21cf),
  [code]({{< code-ref "lectures/Lec07.hs" >}})

  Following on from the end of last time, we introduce `foldr` and
  `foldl` and discussed how they can be seen on lists as rebuilding
  the structure with a new binary operator (instead of `(:)`) and
  termination element (instead of `[]`).

  We then looked at a little bit of theory on data types, particularly
  [sum](https://en.wikipedia.org/wiki/Tagged_union) and
  [product](https://en.wikipedia.org/wiki/Product_type) types.
  Haskell's datatypes are [Algebraic
  Datatypes](https://en.wikipedia.org/wiki/Algebraic_data_type). I
  also pointed to this nice article on [why sum types are nice to
  have](https://chadaustin.me/2015/07/sum-types/).

  We then defined a few more of our own data structures including a
  binary tree and a [rose
  tree](https://en.wikipedia.org/wiki/Rose_tree) (the latter is what
  you get if you import the
  [`Data.Tree`](https://hackage.haskell.org/package/containers-0.6.4.1/docs/Data-Tree.html)
  module).

  We looked at the mapping pattern which lifts a function `a -> b` to
  a function between containers of `as` and `bs` (e.g. `[a] -> [b]`
  for lists), and noticed that the same pattern appears in mapping
  over lots of datatypes. To make this generic, we introduced
  Haskell's
  [`Functor`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#t:Functor)
  type class which implements a generic `fmap`.

  We foreshadowed, but didn't do, an equivalent interface for
  "foldable" datatypes.
