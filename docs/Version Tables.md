As we want to support multiple GHC versions, we need to take care of how to handle the versions of different dependencies.


### GHC vs Base
https://wiki.haskell.org/Base_package
| GHC | Base |
| --- | ---- |
| 8.10.1  | 4.14.0.0 |
| 8.10.2  | 4.14.1.0 |
| 8.10.7  | 4.14.3.0 |
| 9.0.1   | 4.15.0.0 |
| 9.0.2   | 4.15.1.0 |
| 9.2.1   | 4.16.0.0 |
| 9.2.2   | 4.16.1.0 |
| 9.2.5   | 4.16.4.0 |
| 9.4.1   | 4.17.0.0 |
| 9.6.1   | 4.18.0.0 |
| 9.8.1   | 4.19.0.0 |
| 9.10    | 4.20.0.0 |


# Template Haskell
Minimum TH version to support will be 2.16.

### Template Haskell Type Constructors
| Constructor  | Since | Min Base | Min GHC  |
| ----       | --- | --- | --- | 
| `ForallT` | always | 
| `AppT` | always |
| `SigT` | always |
| `VarT` | always |
| `InfixT` | 2.11 | 4.7 | 8.0
| `UInfixT` | 2.11 | 4.7 | 8.0
| `ParensT` | 2.11 | 4.7 | 8.0
| `AppKindT` | 2.15 | 4.11 | 8.8
| `ImplicitParamT` |  2.15 | 4.11 | 8.8
| `ForallVisT` | 2.16 | 4.11 | 8.10.1 
| `PromotedInfixT` | 2.19 | 4.11 | 9.4
| `PromotedUInfixT` | 2.19 | 4.11 | 9.4

Some important version differences:

| TH Version | Issue | Notes |
| --- | --- | --- |
| 2.17 | `TyVarBndr` now has a `flag` parameter, this is a breaking change | 

