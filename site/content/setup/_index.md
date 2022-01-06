---
weight: 1
title: "Setup"
---

# Running a Haskell interpreter

We'll use the [Glasgow Haskell Compiler](https://www.haskell.org/ghc/)
as our Haskell implementation in the course. There are a number of
ways you can access it.

## Via the `mira` timesharing service

You probably obtained a login for the C part of the course last term.
If not, you can [do so
here](https://dur.unidesk.ac.uk/tas/public/ssp/content/serviceflow?unid=f407b0f67eec40c1b76c096be31d0451).
If you're happy using mira you can continue to do so. After logging
in, the Haskell interpreter is available by launching `ghci`.

## Via apps anywhere

On windows managed desktop systems, you can launch the Haskell
platform via apps anywhere.

## On your own device

This may well be preferable to just using a terminal. Your best bet is
probably to follow the official instructions and install
[`ghcup`](https://www.haskell.org/ghcup/install/) and use that to manage
installing `ghc` and any other components. I recommend installing at
least `ghc` and the `haskell-language-server` (for editor
integration).

## Editor support

If you're running remotely on mira there aren't a lot of editors
available, so you can just use `nano`. On your own machine, I suppose
you already have a favourite editor. Many come with extension packages
that make editing Haskell code easier (providing syntax highlighting,
inline error checking and so forth).

{{< hint info >}}
You don't need any of this fancy stuff, but it might make programming
in Haskell more pleasant.
{{< /hint >}}

For [VS Code](https://code.visualstudio.com), install the [Haskell
extension](https://marketplace.visualstudio.com/items?itemName=haskell.haskell).
This will download a bunch of stuff and hook up with your existing
Haskell installation so that you get type-checking hints and
autocompletion.

For [Emacs](https://www.gnu.org/software/emacs/), I recommend LSP, in
particular the [lsp-haskell](https://emacs-lsp.github.io/lsp-haskell/) plugin, which provides
similar functionality to the VS Code plugin.

For other editors, you might find some helpful pointers
[here](https://wiki.haskell.org/IDEs). The best-supported approach is via the [language server
protocol](https://microsoft.github.io/language-server-protocol/),
which is implemented for Haskell
[here](https://github.com/haskell/haskell-language-server). They
provide some advice on [configuring your
editor](https://haskell-language-server.readthedocs.io/en/latest/configuration.html#configuring-your-editor).
This server is installable using `ghcup` on your own device if you're
doing that, unfortunately it's not available on mira.
