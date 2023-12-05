# Revision history for gigaparsec

## 0.2.2.0 -- TBD

* Added `manyMap` and `someMap` combinators.
* Added `filterS` and `mapMaybeS` combinators.
* Added `Text.Gigaparsec.Position` module.
* Added `Text.Gigaparsec.Token` and associated functionality.

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
