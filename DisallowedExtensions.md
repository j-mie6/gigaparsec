# Disallowed Extensions
The following extensions are not compatible with the minimum version of GHC (currently **8.10**)
that we support:

## Syntax

* [**`LexicalNegation`**](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/lexical_negation.html) (9.0)
* [**`QualifiedDo`**](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/qualified_do.html) (9.0)

## Types

* [**`LinearTypes`**](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/linear_types.html) (9.0)
* [**`ImpredicativeTypes`**](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/impredicative_types.html) (9.2)
* [**`TypeData`**](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/type_data.html) (9.6)

## Records

* [**`FieldSelectors`**](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/field_selectors.html) (9.2)
* [**`OverloadedRecordDot`**](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/overloaded_record_dot.html) (9.2): this will be really fun when we can use it!
* [**`OverloadedRecordUpdate`**](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/overloaded_record_update.html) (9.2 _and experimental_)

## Unboxed Types and Primitive Operations

* [**`UnliftedDatatypes`**](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/primitives.html) (9.2)

  This one is a bit annoying to lose, as it would be good to be able to unbox some of the state threaded through the
  parser; in general there is a lot we want to ensure is never lazy -- including state and defunctionalised errors.

  In practice, I think we could probably use this under a `CPP` guard. Worth considering!
