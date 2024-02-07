# Revision history for gigaparsec

## 0.2.5.1 -- 2024-02-07
* Fixed bug where hints can be revived by the `hide` combinator.

## 0.2.5.0 -- 2024-02-04
* Supported `Text.Gigaparsec.Errors.TokenExtractors`, which allows for recipes for token extraction
  during error message creation.

## 0.2.4.1 -- 2024-02-04
* Fixed infinite loop in lexer arising from forcing a knot-tie, the knot has been massaged out.

## 0.2.4.0 -- 2024-02-03
* Added `ErrorConfig` and related types, along with `mkLexerWithErrorConfig`, to now allow
  for custom lexing errors.

## 0.2.3.0 -- 2024-01-29
* Added _Verified Errors_ and _Preventative Errors_ in `Text.Gigaparsec.Errors.Patterns`.

## 0.2.2.3 -- 2024-01-29
* Fixed bug where `markAsToken` doesn't apply at the correct offsets.

## 0.2.2.2 -- 2024-01-29
* Optimised the error system using `DefuncError` and `DefuncHints`.
* Fixed bugs with amending and token merging.

## 0.2.2.1 -- 2024-01-29
* Fixed bug where case sensitive keywords where parsed insensitively and vice-versa.

## 0.2.2.0 -- 2024-01-21

* Added `manyMap` and `someMap` combinators.
* Added `filterS` and `mapMaybeS` combinators as well as `ErrorGen` and more general combinators.
* Added `Text.Gigaparsec.Position` module.
* Added `Text.Gigaparsec.Token` and associated functionality.
* Added `Text.Gigaparsec.Patterns` module, which is currently heavily experimental.

## 0.2.1.0 -- 2023-11-14

* Added `Text.Gigaparsec.Debug`, which provides debugging capabilities.
* Added `parseFromFile` for quick parsing from files.
* Added additional documentation.

## 0.2.0.0 -- 2023-11-09

* Added error system.
* `parse` now has a type parameter, `parse @String` restores old behaviour.
* for convenience `parseRepl` will print a parse to the terminal with the `String` error messages.

## 0.1.0.0 -- 2023-10-17

* First version. Released on an unsuspecting world.
