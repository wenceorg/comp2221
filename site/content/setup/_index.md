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

## On your own device

This may well be preferable to just using a terminal. You probably
want to install the [haskell platform](https://www.haskell.org/platform/)

On Debian based systems (for example Ubuntu), you'll want to install
the `haskell-platform` package. For other Linux distributions, see the
[haskell platform page](https://www.haskell.org/platform/linux.html).

On MacOS, your best bet is via [Homebrew](https://brew.sh), which I
recommend as a package manager for MacOS systems in general. You
should `brew install haskell-stack`.

On Windows, follow the instructions given on the [haskell platform
page](https://www.haskell.org/platform/windows.html).

{{< hint info >}}
For other options, see the documentation
[here](https://www.haskell.org/downloads/).
{{< /hint >}}

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
[here](https://wiki.haskell.org/IDEs). The most recent approach
appears to be via the [language server
protocol](https://microsoft.github.io/language-server-protocol/),
which is implemented for Haskell
[here](https://github.com/haskell/haskell-language-server). They
provide some advice on [configuring your
editor](https://github.com/haskell/haskell-language-server#configuring-your-editor).
