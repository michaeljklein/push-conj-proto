# push-conj-proto

Prototypes for pushing conjunctions through logical expressions.

Expressions are represented as a `Free` `Monad` of a sum of base `Functor`s:

```haskell
type ExprF = Free (And :+: Or :+: Xor :+: Not)
```

We use:

```haskell
getAnd :: ExprF a -> Either (AndF a) (ExprF (AndF a))
getAndF :: ExprF a -> ExprF (AndF a)
```

To split out the "leafy subtrees"<sup>[1](#leafysubtrees)</sup> composed of `And`s, then




```haskell
partAnd :: ExprF (AndF a) -> ExprF (ExprF (AndF a))
```

to partition the outer expression on the `And`-filled leaves.

## Note

<a name="myfootnote1">1</a>: By "leafy subtrees", we mean:

_Definition_: A subtree `S` of a tree `T` is _leafy_ if
the set of leaves of `S`, `leaves(S)` is a subset of the
set of leaves of `T`: `leaves(S) <= leaves(T)`


### Example 1

The leafy subtrees of a `NonEmpty` list are just:

```haskell
mapMaybe nonEmpty . toList . tails :: NonEmpty a -> [NonEmpty a]

λ> (mapMaybe nonEmpty . toList . tails) [1..3]
[1 :| [2,3],2 :| [3],3 :| []]
```


### Example 2

Consider the following trees:

```
T :=
   1
   /\
  2  3
 /\   \
4  5   6
  /\   /
 7  8 9

S1 :=
  2
 /\
4  5
  /\
 7  8

S2 :=
   1
   /\
  2  3
 /\
4  5
  /\
 7  8 


leaves(T) = {4, 7, 8, 9}
leaves(S1) = {4, 7, 8}
leaves(S2) = {4, 7, 8, 3}
```

`S1` is a leafy subtree of `T`, but `S2` is not since `3` is not an
element of `leaves(T)`.


# Docs

Haddock generated documentation may be found [here](https://michaeljklein.github.io/push-conj-proto/)

