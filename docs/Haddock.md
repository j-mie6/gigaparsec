# Conditional Compilation
To improve documentation of re-exports (such as `<|>`), we use CPP macros that re-define these re-exports only during a `haddock` pass.
We achieve this with the `__HADDOCK_VERSION__` macro, so we can write things like,
```haskell
#ifdef __HADDOCK_VERSION__
{-| Some custom docs
-}
infixl 3 <|>
(<|>) :: Parsec a -- ^ custom docs.
      -> Parsec a -- ^ custom docs.
      -> Parsec a -- ^ custom docs.
(<|>) = (Applicative.<|>)
#endif
```

If we were to perform the redefinition for the actual library, then users would have conflicting types between 
`Text.Gigaparsec.<|>` and the applicative `<|>`.
Moreover, the haskell compiler will treat these as different, and we may lose some specialisation improvements/optimisations.

One side-effect of this is that `<|>` within the `gigaparsec` library must be imported from `Applicative`, not from `Text.Gigaparsec`, 
otherwise the `haddock` phase of compilation will fail in cases where `<|>` is used in a type that is not `Parsec`.
This is a small price to pay for better documentation :)
This does not affect external users.
