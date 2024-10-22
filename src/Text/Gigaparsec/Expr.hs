{-# LANGUAGE Safe #-}
{-# LANGUAGE GADTs #-}
{-|
Module      : Text.Gigaparsec.Expr
Description : This module contains various functionality relating to the parsing of expressions.
License     : BSD-3-Clause
Maintainer  : Jamie Willis, Gigaparsec Maintainers
Stability   : experimental

This module contains various functionality relating to the parsing of expressions.

It also includes functionality for constructing larger precedence tables, 
which may even vary the type of each layer in the table, 
allowing for strongly-typed expression parsing.
-}
module Text.Gigaparsec.Expr (
  -- ** Infix combinators
  {-|
  These are standalone Infix combinators. 
  They are useful for parsing the application of operators to values in many different configurations, including: 
  prefix, postfix, infix (left and right associated), and mixed fixity. 

  Although these combinators do have their uses, 
  they are overshadowed by the 'precedence' combinator, 
  which allows for the combining of multiple levels of infix-chaining in a clean and concise way.
  -}
  infixl1, infixr1, infixn1, prefix, postfix,
  -- ** Precedence Combinators
  {-|
  These combinators represent each of the different "shapes" of precedence combinator, 
  which takes a description of the precedence relations between various operators and constructs a parser for them.
  -}
  precedence,
  precedence',
  -- ** Precedence Layer Builders
  {-|
  These are used to construct an individual layer of a precedence table. 
  They all allow for the tying of a 'Fixity' to the parsers that inhabit the level, 
  which will have type @'Op' a b@ for some @a@ (the type of the layer below) and @b@ (the type of this layer). 
  'Op' uses existential types, so that the types of the parsers are only known when fixity itself is known. 
  
  The different objects represent different inter-layer relationships: 

    * are they the same type? Use 'ops'; 
    * is the layer below a subtype of this layer? use 'sops'; 
    * or none of the above? Use 'gops'!
  -}
  ops, sops, gops,
  -- ** Fixity Description
  {-|
  These all describe the fixities and associativities of operators at a given level of the precedence table. 
  They are special because they each encode an 'Op' type, 
  which directs the type of the operators that are legal at the level (using existential types).
  -}
  Fixity(InfixL, InfixR, InfixN, Prefix, Postfix),
  -- ** Precedence Table Datatypes
  {-|
  These are the parts that make up a precedence table
   (in particular, they are used for heterogeneous expression parsing, 
   with the types of each layer of the table vary from one another). 
   
  These are (mostly) not constructed directly,
  but are instead constructed via the use of the Ops builders or the :+ and +: methods.
  -}
  Op(Op),
  Prec(Atom, Level),
  (>+),
  (+<),
  -- ** Module Re-export
  {-|
  This should be removed at some point.
  -}
  module Text.Gigaparsec.Expr) where

import Text.Gigaparsec (Parsec)
import Text.Gigaparsec.Combinator (choice)
import Text.Gigaparsec.Expr.Infix (infixl1, infixr1, infixn1, prefix, postfix)
import Text.Gigaparsec.Expr.Subtype (Subtype(upcast))

import Data.List (foldl')

{-|
Denotes the fixity and associativity of an operator. 

Importantly, it also specifies the type of the operations themselves.
-}
type Fixity :: * -> * -> * -> *
data Fixity a b sig where
  -- | A left-associative binary Operator.
  InfixL  :: Fixity a b (b -> a -> b)
  -- | A right-associative binary Operator.
  InfixR  :: Fixity a b (a -> b -> b)
  -- | A non-associative binary Operators.
  InfixN  :: Fixity a b (a -> a -> b)
  -- | A Unary prefix operator.
  Prefix  :: Fixity a b (b -> b)
  -- | A Unary postfix operator.
  Postfix :: Fixity a b (b -> b)

{-|
Describes a single layer of operators in the precedence tree.

The parameters are:

* @a@: the base tyep consumed by the operators.
* @b@: the type produced/consumed by the operators.
-}
type Op :: * -> * -> *
data Op a b 
  {-| 
  Describes the operators at a specific level in the precedence tree, 
  such that these ops consume @b@s, possibly @a@s and produce @b@s: 
  this depends on the 'Fixity' of the operators.
  -}
  = forall sig. Op (Fixity a b sig) (a -> b) (Parsec sig)

{-|
The base type for precedence tables.

The parameter @a@ is the type of structure produced by the list of levels.

For more complex expression parser types, 'Prec' describes the precedence table whilst preserving the intermediate structure between each level.

The base of the table will always be an 'Atom', and each layer built on top of the last using the 'Level' constructor.
-}
type Prec :: * -> *
data Prec a where
  {-|
  Adds a new layer to this precedence table on the left, in a weakest-to-tightest ordering.

  This method associates to the left, so left-most applications are tighter binding 
  (closer to the atoms) than those to the right.

  It should not be mixed with '(+<)', as this can be create a confusing and less predictable precedence table.
  -}
  Level :: Prec a -> Op a b -> Prec b
  {-|
  The base of a precedence table.

  Forms the base of a precedence table, requiring at least one atom to be provided. This first atom will be parsed first.
  -}
  Atom  :: Parsec a -> Prec a

{-|
Adds a new layer to this precedence table on the left, in a weakest-to-tightest ordering.
This is an alias for 'Level'.

This method associates to the left, so left-most applications are tighter binding 
(closer to the atoms) than those to the right.

It should not be mixed with '(+<)', as this can be create a confusing and less predictable precedence table.
-}
infixl 5 >+
(>+) :: Prec a -- ^ the lower precedence table.
     -> Op a b -- ^ the operators that make up the new level on the table.
     -> Prec b -- ^ a new table that incorporates the operators and atoms in the given table, along with extra ops.
(>+) = Level


{-|
Adds a new layer to this precedence table on the right, in a tightest-to-weakest ordering.

This method associates to the right (with this table on the right!), 
so right-most applications are tighter binding (closer to the atoms) than those to the left. 

It should not be mixed with '(>+)', as this can be create a confusing and less predictable precedence table.
-}
infixr 5 +<
(+<) :: Op a b -- ^ the lower precedence table. 
     -> Prec a -- ^ the operators that make up the new level on the table. 
     -> Prec b -- ^ a new table that incorporates the operators and atoms in the given table, along with extra ops.
(+<) = flip (>+)

{-|
Constructs a precedence parser.

This combinator builds an expression parser given a heterogeneous precedence table.

An expression parser will be formed by collapsing the given precedence table layer-by-layer. 
Since this table is heterogeneous, each level of the table produces a difference type, 
which is then consumed by the next level above.
-}
precedence  :: Prec a   -- ^ the description of the heterogeneous table, where each level can vary in output and input types.
            -> Parsec a -- ^ an expression parser for the described precedence table.
precedence (Atom atom) = atom
precedence (Level lvls lvl) = con (precedence lvls) lvl
  where con :: Parsec a -> Op a b -> Parsec b
        con p (Op InfixL wrap op) = infixl1 wrap p op
        con p (Op InfixR wrap op) = infixr1 wrap p op
        con p (Op InfixN wrap op) = infixn1 wrap p op
        con p (Op Prefix wrap op) = prefix wrap op p
        con p (Op Postfix wrap op) = postfix wrap p op

{-|
This combinator builds an expression parser given a collection of homogeneous atoms and operators.

An expression parser will be formed by treating the given parser as the base 'Atom', 
then each successive layer in the given list as another 'Level' in a tightest-to-weakest binding.
This means the last element of the given list will be at the top of the precedence table.

Each level must consume and produce the same type.
-}
precedence' :: Parsec a -- ^ The atom at the base of the table.
            -> [Op a a] -- ^ The levels of operators, oredered tightest-to-weakest.
            -> Parsec a -- ^ An expression parser for the constructed precedence table.
precedence' atom = precedence . foldl' (>+) (Atom atom)

{-|
Constructs a single layer of operators @Op a b@, supporting fully heterogeneous precedence parsing.

This function builds an Ops object representing many operators found at the same precedence level, with a given fixity.

The operators found on the level constructed by this function are heterogeneous: 
the type of the level below may vary from the types of the values produced at this level. 
To make this work, a function must be provided that can transform the values from the 
layer below into the types generated by this layer.

The fixity describes the shape of the operators expected.
-}
gops  :: Fixity a b sig -- ^ the fixity of the operators described.
      -> (a -> b)       -- ^ the function that will convert @a@s to @b@s.
                        -- this will be at right of a left-assoc chain, left of a right-assoc chain, or the root of a prefix/postfix chain.
      -> [Parsec sig]   -- ^ The operators themselves, provided variadically.
      -> Op a b         -- ^ A layer in the precedence table, containing the given parsers.
gops fixity wrap = Op fixity wrap . choice

{-|
Constructs a single layer of operators in the precedence tree.
-}
ops :: Fixity a a sig -- ^ The fixity of the layer to add.
    -> [Parsec sig]   -- ^ The parsers that can be run in this layer.
    -> Op a a         -- ^ A layer in the precedence table, containing the given parsers.
ops fixity = gops fixity id

{-|
Constructs a single layer of operators in the precedence tree for values of @Op a b@, where @a@ is a 'Subtype' of @b@.
This provides support for subtyped heterogeneous precedence parsing.
-}
sops :: Subtype a b
     => Fixity a b sig -- ^ The fixity of the layer to add.
     -> [Parsec sig]   -- ^ The parsers that can be run in this layer.
     -> Op a b         -- ^ A layer in the precedence table, containing the given parsers.
sops fixity = gops fixity upcast
