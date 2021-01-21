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
