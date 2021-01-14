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
