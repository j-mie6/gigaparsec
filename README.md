# Gigaparsec ![GitHub Workflow Status][Badge-Status] [![GitHub license][Badge-License]][License] [![Badge-Haddock]][Link-Haddock] ![Semantic Versioning: pvp][Badge-PVP]

`Gigaparsec` is a refreshed implementation of a parser combinator library in the `parsec` style.
It has the following aims:

1. Be **approachable** for beginners -- both those new
  to parser combinators and new to Haskell itself. Good documentation and resources are a key priority.
1. Be **complete**, with a well-rounded API including
  support for *highly-configurable* lexing combinators,
  and generic expression parsing functionality.
1. Be **modern**, with first-class support for the patterns outlined in [*Design Patterns for Parser Combinators (Functional Pearl)*][DPfPC]. It also strives for API compatibility with Scala [`parsley`][Scala-Parsley], to allow for easy porting of both
parsers, knowledge, and learning materials.
1. Be **efficient** -- while it may not be quite as fast as `megaparsec`, we hope that it will still remain reasonably efficient, as least more than `parsec`, whilst still providing additional benefits.

To achieve these aims, some departures have been made
from many of the classic `parsec`-style implementations:

* `gigaparsec`'s `Parsec` type is simple: only one type-parameter. There are a few consequences of this:
    - `Parsec` does not have a `ParsecT` variant: occasionally, people may wish to thread other monads through a parser, however, most of the practical use-cases of this can be captured using `gigaparsec`'s *registers*. The exception here is `IO`
    operations.
    - `Parsec` cannot generate the type of the input stream: currently, the input is assumed to be `String`. In future, however, it may be possible to specify an implementation for the input without resorting to noise in the type-signatures.
    - `Parsec` cannot specify a custom error type to be carried through parsing. In practice, a rich typeclass-driven API can be used to format error messages in the desired way, as well as change their types at the point of calling `parse`.
* The common `try` combinator has been more appropriately
  named as the `atomic` combinator: this is consistent with the `parsley` naming, and we believe that this name is more true to the combinators purpose.
* Creating new primitive combinators is not endorsed by the libary: while the capacities to make them have been exposed, they are not made part of the public API to allow freedom for the maintainers to continue to innovate and improve the internals -- this includes continued optimisation.

Current `HEAD` documentation can be found [here][Link-Haddock]: for
stable documentation please consult Hackage directly.

[DPfPC]: https://dl.acm.org/doi/10.1145/3471874.3472984
[Scala-Parsley]: https://github.com/j-mie6/parsley
[Link-Haddock]: https://j-mie6.github.io/gigaparsec
[Badge-Haddock]: https://img.shields.io/badge/documentation-available-green
[Badge-Status]: https://img.shields.io/github/actions/workflow/status/j-mie6/gigaparsec/ci.yaml?branch=main
[Badge-License]: https://img.shields.io/github/license/j-mie6/parsley.svg
[License]: https://github.com/j-mie6/gigaparsec/blob/main/LICENSE
[Badge-PVP]: https://img.shields.io/badge/version%20policy-pvp-blue
