---
title: Slides & commentary
draft: false
weight: 2
---

# Lecture slides and video links

As the course progresses, I'll add the annotated slides and live code
examples, along with links to the videos (accessible with a Durham
account), and some commentary.

- 2022-01-10: [Annotated slides]({{< static-ref
  "slides/2021-22/Lec01.pdf" >}}),
  [video](https://durham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=cefca3c8-328b-4b0e-b0a1-ae19011c7e90), [code]({{< code-ref "lectures/2021-22/Lec01.hs" >}})
  
  We got most of the way through the slides, and then did some live
  examples. We same some very simple functions and probably quite a
  lot of syntax that we'll go through in more detail as the course
  progresses.
  
  My editor was a bit angry fruit salad, which I've toned
  down for next time.
  
  I would encourage you to get a fancy editor setup so that you too
  can take advantage of the "code actions" I was using, these are
  provided through
  [LSP](https://microsoft.github.io/language-server-protocol/) by the
  [Haskell wingman](https://haskellwingman.dev).

- 2022-01-14: [Annotated slides]({{< static-ref
  "slides/2021-22/Lec02.pdf" >}}),
  [video](https://durham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=cc24733d-6605-4282-9b8f-ae1d00eaa76f),
  [code]({{< code-ref "lectures/2021-22/Lec02.hs" >}})
  
  We finished off by going over the definition of `filter` we had seen
  at the end of the last session. I've updated the lecture 1 annotated
  slides above to include my new annotations (I wrote it out more
  neatly).
  
  We then did some introductory types and started hinting at the idea
  that function types, and especially higher-order functions, are very
  important. We got about halfway through the slides, and I'll pick up
  there next time.

  [Gary Bernhardt](https://www.destroyallsoftware.com/) has a fun talk
  about type errors, mostly in
  [javascript](https://www.destroyallsoftware.com/talks/wat).

- 2022-01-17: [Annotated slides]({{< static-ref
  "slides/2021-22/Lec03.pdf" >}}),
  [video](https://durham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=e4009fa3-8fb3-4bd2-b1fe-ae20011e6bba),
  [code (finishing session 2)]({{< code-ref
  "lectures/2021-22/Lec02.hs" >}}) and [starting session 3]({{< code-ref
  "lectures/2021-22/Lec03.hs" >}})
  
  We finished off a few slides from session 2 (and I updated the
  annotated ones above). Then we spent quite a lot of time looking at
  defining functions and in particular discussing
  [currying](https://en.wikipedia.org/wiki/Currying) and why Haskell
  tends to prefer functions written in curried form.
  
  Then I showed a few session 3 slides and talked about polymorphic
  functions and monomorphisation. We'll pick up there next time and
  continue with session 3 stuff.
  
- 2022-01-21:
  [video](https://durham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=6e60d837-300c-4453-b60c-ae2400eaa0c9),
  [code (finishing session 3)]({{< code-ref
  "lectures/2021-22/Lec03.hs" >}})

  We didn't see any slides this time and instead worked our way
  through the definitions of some polymorphic functions. The main
  focus was on figuring out how we might be able to write [total
  functions](https://en.wikipedia.org/wiki/Partial_function) which are
  functions which produce valid results for all possible inputs. To do
  so, we introduced the
  [`Maybe`](https://hackage.haskell.org/package/base-4.16.0.0/docs/Prelude.html#t:Maybe)
  data type, which models a computation that may fail.
  
  We then saw that to constructively work with `Maybe` types we are
  likely going to need a way to transform values that live "inside"
  `Maybe`s and introduced a function `applyInsideMaybe` whose type signature bore a
  striking resemblance to that of
  [`map`](https://hackage.haskell.org/package/base-4.16.0.0/docs/Prelude.html#v:map),
  which is approximately where we will start next time.

- 2022-01-24:
  [annotated slides (updated from session 3)]({{< static-ref
  "slides/2021-22/Lec03.pdf" >}}),
  [video](https://durham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=39af24fe-316b-42a7-b0e2-ae27011c4f92),
  [code]({{< code-ref "lectures/2021-22/Lec04.hs" >}})
  
  We talked a little bit about Haskell type classes and how they are
  used to implement constrained generic programming: that is writing
  generic functions that can specify interfaces that the arguments
  should satisfy.
  
  The [wikipedia
  page](https://en.wikipedia.org/wiki/Polymorphism_(computer_science))
  has a nice overview as usual on polymorphism. The classic work on
  subtyping is from [Barbara
  Liskov](https://en.wikipedia.org/wiki/Barbara_Liskov) from 1987. The
  method that Haskell uses for constrained (ad-hoc) polymorphism based
  on type classes was introduced by [Phil
  Wadler](http://homepages.inf.ed.ac.uk/wadler/) and [Stephen
  Blott](https://www.computing.dcu.ie/~sblott/) in [_How to make
  ad-hoc polymorphism less ad
  hoc_](http://homepages.inf.ed.ac.uk/wadler/topics/type-classes.html#class).
  [Here's a video](https://www.youtube.com/watch?v=6COvD8oynmI) of
  Simon Peyton Jones giving an introductory talk on type classes and
  their implementation in GHC.
  
  We implemented our own linked list type and and discussed pattern
  matching a bit more.

- 2022-01-28:
  [video](https://durham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=a9386ccd-4791-4d18-a6fb-ae2b00eeb084),
  [code]({{< code-ref "lectures/2021-22/Lec05.hs" >}})
  
  We just did code today, in particular we systematically looked at a
  method for writing recursive functions. We have effectively covered
  the slides up to lecture 5 (as uploaded on blackboard), we just
  started hinting at folds in the last part of the session 5 slides.
  
  We saw that a number of "library" functions on lists follow the same
  pattern, and we sketched a higher order function that captures this
  pattern.
  
  I showed a way to find functions in the Haskell standard
  library (and packages) if you know the type by using
  [hoogle](https://hoogle.haskell.org).
  
- 2022-01-31:
  [annotated slides]({{< static-ref "slides/2021-22/Lec07.pdf" >}}),
  [video](https://durham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=c07d3261-2522-46af-b2b5-ae2e0119a7b9),
  no code today
  
  We introduced, following on from looking for patterns, `foldr` and
  `foldl` and saw how they can be seen on lists as rebuilding the
  structure with a new binary operator (instead of `(:)`) and initial
  element (instead of `[]`).
  
  We then also looked at two "principled" type classes, namely
  [`Functor`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#t:Functor)
  for mappable types, and
  [`Foldable`](https://hackage.haskell.org/package/base-4.16.0.0/docs/Prelude.html#t:Foldable),
  for foldable types.
  
  The type classes are termed "principled" because their
  implementations are expected to satisfy certain equational laws that
  mathematically capture certain properties that an implementation
  must obey so that it behaves "as expected". We saw the `Functor`
  laws, but not the `Foldable` laws (which need more time than we have
  here). Note that although I often ask you to implement `Functor` and
  `Foldable` instances, GHC can actually derive them for you if you
  add
  ```hs
  {-# LANGUAGE DeriveFunctor #-}
  {-# LANGUAGE DeriveFoldable #-}
  ```
  at the top of your files.
  
  I've diverged a bit from the slides uploaded to blackboard, although
  I have covered the material in a combination of the slides you've
  seen and the live code: you may wish to browse the slides as well to
  compare with the code we've seen.

- 2022-02-04:
  [annotated slides]({{< static-ref "slides/2021-22/Lec08.pdf" >}}),
  [code]({{< code-ref "lectures/2021-22/Lec08.hs" >}}),
  [video](https://durham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=59cd3222-5c2f-463e-858d-ae3200ea66dd)

  I finished the declarations for `Functor` and `Foldable` instances
  for the `Maybe`, `BinaryTree`, and `RoseTree` data types we'd seen.
  Then we talked about how Haskell evaluations expression graphs and
  how we can do strict (eager) evaluation with `($!)`. I rushed
  through the last few slides a little bit so will go over them again
  briefly on Monday to wrap up the lazy evaluation aspects and then
  talk about data encapsulation and compile-time safe APIs.

- 2022-02-07:
  [annotated slides]({{< static-ref "slides/2021-22/Lec09.pdf" >}}),
  [code]({{< code-ref "lectures/2021-22/Lec09.hs" >}}) (and [main
  file]({{< code-ref "lectures/2021-22/Main.hs" >}}),
  [video](https://durham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=c16f703f-b397-4865-99c4-ae35011d2515)
  
  I showed differences between strict and lazy evaluation for some
  simple folds in terms of performance (particularly between `foldl`
  and `foldl'`), though noted for this simple example that GHC does a
  good job determining that it can evaluate things strictly. For more
  details on this, see the [haskell
  wiki](https://wiki.haskell.org/Performance/Strictness).
  
  I then talked about the design of APIs and "type-driven design", and
  how a rich type system can help us in the design of libraries that
  are in some sense impossible to misuse.
  
  This style of interface is becoming more popular because it
  pushes a bunch of the complexity of keeping track of invariants onto
  the type system and compiler (rather than the programmer's brain).
  
  In Haskell-ese an often quoted mantra is to ["make illegal states
  unrepresentable"](https://buttondown.email/hillelwayne/archive/making-illegal-states-unrepresentable/).
  
  If you're interested in this kind of stuff, here are some starting
  points for further reading
  
  - [Alexis King](https://lexi-lambda.github.io) has a nice article on
    the idea of [parsing rather than
    validating](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/)
  - The [Rust](https://www.rust-lang.org/) community has really got on
    board with [type state
    patterns](http://cliffle.com/blog/rust-typestate/)
  - [Matt Noonan](https://mobile.twitter.com/banjotragedy) shows how
    Haskell can provide enough dependent types to encode quite
    complicated invariants (such as the `mergeBy` example in the
    lecture) [in the type system](https://kataskeue.com/gdp.pdf).
  - If you're interested in formal methods and proof systems, [Talia
    Ringer](https://dependenttyp.es/) has an introductory course on
    [proof
    automation](https://dependenttyp.es/classes/598sp2022.html), and
    [Kevin Buzzard](https://www.imperial.ac.uk/people/k.buzzard)
    writes a lot about [formalising
    mathematics](https://xenaproject.wordpress.com) in
    [Lean](https://xenaproject.wordpress.com).
