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

To split out the leaves composed of `And`s, then

```haskell
partAnd :: ExprF (AndF a) -> ExprF (ExprF (AndF a))
```

to partition the outer expression on the `And`-filled leaves.


# Docs

Haddock generated documentation may be found [here](https://michaeljklein.github.io/push-conj-proto/)

