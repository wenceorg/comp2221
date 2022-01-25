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
  [video](https://durham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=39af24fe-316b-42a7-b0e2-ae27011c4f92), 
  [annotated slides (updated from session 3)]({{< static-ref
  "slides/2021-22/Lec03.pdf" >}}), [code]({{< code-ref
  "lectures/2021-22/Lec04.hs" >}})
  
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
