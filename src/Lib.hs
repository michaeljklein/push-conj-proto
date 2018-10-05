{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import Data.Proxy
import Data.Functor.Classes
import Control.Monad.Free
import Control.Monad
import Control.Monad.Trans.Free (FreeT(..))
import qualified Control.Monad.Trans.Free as T
import GHC.Generics
import Data.Functor.Apply

-- | GADT implementation of logical expressions
--
-- @
-- data Expr a where
--   Lit :: Bool -> Expr Bool
--   And :: Expr Bool -> Expr Bool -> Expr Bool
--   Or  :: Expr Bool -> Expr Bool -> Expr Bool
--   Xor :: Expr Bool -> Expr Bool -> Expr Bool
--   Not :: Expr Bool -> Expr Bool
-- @
--
gadtExpr :: ()
gadtExpr = ()

data And a = And a a deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)
data Or  a = Or a a  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)
data Xor a = Xor a a deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)
data Not a = Not a deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

instance Eq1 And where
  liftEq eq ~(And x y) ~(And z w) = eq x z && eq y w

instance Eq1 Or where
  liftEq eq ~(Or x y) ~(Or z w) = eq x z && eq y w

instance Eq1 Xor where
  liftEq eq ~(Xor x y) ~(Xor z w) = eq x z && eq y w

instance Eq1 Not where
  liftEq eq ~(Not x) ~(Not z) = eq x z

instance (Eq1 f, Eq1 g) => Eq1 (f :+: g) where
  liftEq eq (L1 x) (L1 y) = liftEq eq x y
  liftEq eq (R1 x) (R1 y) = liftEq eq x y
  liftEq _ _ _ = False

instance Show1 And where
  liftShowsPrec sp _ n ~(And x y) = showsBinaryWith sp sp "And" n x y

instance Show1 Or where
  liftShowsPrec sp _ n ~(Or x y) = showsBinaryWith sp sp "Or" n x y

instance Show1 Xor where
  liftShowsPrec sp _ n ~(Xor x y) = showsBinaryWith sp sp "Xor" n x y

instance Show1 Not where
  liftShowsPrec sp _ n ~(Not x) = showsUnaryWith sp "Not" n x

instance Apply And where
  And f g <.> And x y = And (f x) (g y)

instance Apply Or where
  Or f g <.> Or x y = Or (f x) (g y)

instance Apply Xor where
  Xor f g <.> Xor x y = Xor (f x) (g y)

instance Apply Not where
  Not f <.> Not x = Not (f x)

-- instance Apply f => MonadZip (Free f) where
--   mzip (Pure x) y = (x ,) <$> y
--   mzip x (Pure y) = (, y) <$> x
--   mzip ~(Free x) ~(Free y) = liftF2 mzip x y

-- instance MonadZip (Free (And :+: Or :+: Xor :+: Not)) where
--   mzip (Pure x) y = (x ,) <$> y
--   mzip x (Pure y) = (, y) <$> x
--   mzip (Free (L1 x)) (Free (L1 y)) = _ x y

-- instance MonadZip (Free (And :+: (Or :+: (Xor :+: Not)))) where



type AndF = Free And

-- | Example lowest-level optimization:
--
-- @
--  x \``And`\` x = x
-- @
--
simplAndEqArgs :: Eq a => And a -> Either a (And a)
simplAndEqArgs ~(And x y) =
  if x == y
    then Left x
    else Right $ x `And` y

-- | Lift with the given function, collapsing and reapplying when `Left` is returned
liftCollapse ::
     (Functor f, c (Free f a))
  => proxy c
  -> (forall t. c t =>
                  f t -> Either t (f t))
  -> Free f a
  -> FreeT f (Either a) a
liftCollapse _ _ (Pure x) = FreeT $ Left x
liftCollapse prxy f (Free xs) =
  FreeT .
  either
    (runFreeT . liftCollapse prxy f)
    (Right . T.Free . fmap (liftCollapse prxy f)) $
  f xs

simplAndEqArgsF :: Eq a => AndF a -> AndF a
simplAndEqArgsF = reduceSimpl . liftCollapse (Proxy :: Proxy Eq) simplAndEqArgs

reduceSimpl :: Functor f => FreeT f (Either a) a -> Free f a
reduceSimpl (FreeT (Left x)) = Pure x
reduceSimpl (FreeT (Right x)) =
  case x of
    T.Pure x' -> Pure x'
    T.Free x' -> Free $ reduceSimpl <$> x'


type OrF  = Free Or
type XorF = Free Xor
type NotF = Free Not


type ExprF = Free (And :+: Or :+: Xor :+: Not)

getAnd :: ExprF a -> Either (AndF a) (ExprF (AndF a))
getAnd (Pure x) = Left $ Pure x
getAnd (Free (L1 (And x y))) =
  case getAnd x of
    Left x' ->
      case getAnd y of
        Left y' -> Left . wrap $ x' `And` y'
        Right y' -> Right . wrap . L1 $ fmap return (fromAndF x') `And` y'
    Right x' -> Right . wrap . L1 $ x' `And` fmap return y
getAnd (Free (R1 x)) =
  case x of
    L1 (Or y z) -> Right . wrap . R1 . L1 $ Or (getAndF y) (getAndF z)
    R1 x' ->
      case x' of
        L1 (Xor y z) ->
          Right . wrap . R1 . R1 . L1 $ Xor (getAndF y) (getAndF z)
        R1 (Not y) -> Right . wrap . R1 . R1 . R1 . Not $ getAndF y

getAndF :: ExprF a -> ExprF (AndF a)
getAndF = either return id . getAnd

fromAndF :: AndF a -> ExprF a
fromAndF (Pure x) = Pure x
fromAndF (Free (And x y)) = wrap . L1 $ fromAndF x `And` fromAndF y

partAnd :: ExprF (AndF a) -> ExprF (ExprF (AndF a))
partAnd (Pure x) = Pure $ Pure x
partAnd (Free (L1 (And (Pure x) (Pure y)))) = Pure . Pure . wrap $ And x y
partAnd (Free (L1 (And x y))) = case getAnd x of
                                  Left x' -> case getAnd y of
                                               Left y' -> Pure . Pure . wrap $ join x' `And` join y'
                                               Right y' -> wrap . L1 $ And (Pure . Pure . join $ x') (Pure $ join <$> y')
                                  Right x' -> wrap . L1 $ And (Pure $ join <$> x') (Pure y)
partAnd (Free (R1 (L1 (Or x y)))) =
  case getAnd x of
    Left x' ->
      case getAnd y of
        Left y' -> Pure . wrap . R1 . L1 $ Or (Pure $ join x') (Pure $ join y')
        Right y' ->
          wrap . R1 . L1 $ Or (Pure . Pure $ join x') (Pure . join <$> y')
    Right x' -> wrap . R1 . L1 $ Or (Pure . join <$> x') (partAnd y)
partAnd (Free (R1 (R1 (L1 (Xor x y))))) =
  case getAnd x of
    Left x' ->
      case getAnd y of
        Left y' ->
          Pure . wrap . R1 . R1 . L1 $ Xor (Pure $ join x') (Pure $ join y')
        Right y' ->
          wrap . R1 . R1 . L1 $ Xor (Pure . Pure $ join x') (Pure . join <$> y')
    Right x' -> wrap . R1 . R1 . L1 $ Xor (Pure . join <$> x') (partAnd y)
partAnd (Free (R1 (R1 (R1 (Not x))))) =
  either
    (Pure . wrap . R1 . R1 . R1 . Not . Pure . join)
    (wrap . R1 . R1 . R1 . Not . partAnd . fmap join) $
  getAnd x

-- the innermost ExprF is And-less
type AndO = Free (Or :+: Xor :+: Not)

-- | Part and dart notes
--
-- @
--  part :: [] . Maybe -> [] . (Maybe . NonEmpty) . Maybe
--  part ~ map Just . groupOn isJust
--
--  dart :: [] . (Maybe . NonEmpty) . Maybe -> [] . Maybe . NonEmpty
--  dart ~ map (fmap (fmap fromJust))
--
--  pver :: [] . Maybe -> [] . Maybe . []
--  pver ~ fmap (fmap toList) . dart . part
-- @
--
-- An expression type is (up to) the recursive completion of the exclusive or (non-inclusive sum) of two expression types.
--
-- @
--  A functor is (up to) isomorphic to the recursive completion of the `xor` of two other functors.
--    f ~ g `xor` h
--  With the boundary language introduced before, we may bound the "leafy" subtrees that completely consist of one functor.
--    This gives us: f -> f . g (|| f -> f . h)
--  Next, we split out the trees that contain those, and only the other functor:
--    f . g -> f . h . g
--  Finally we repeat the process for the original "sub" functor:
--    f . h . g -> f . g . h . g
--  Alternatively, we can think of the pattern: "^.*GHG$", where "G", "H" represent the matcher of a single element of a path in g, h, resp.
-- @
partDartPver :: ()
partDartPver = ()


-- |
--
-- @
--  Expr = ExprL `xor` ExprR
--  splitL :: Expr -> Expr . ExprL
--  splitR :: Expr -> Expr . ExprR
--  splitRL :: Expr . ExprR -> Expr . (ExprR . ExprL) . ExprR
--
--  splitLR :: Expr . ExprL -> Expr . (ExprL . ExprR) . ExprL
--  assoc :: Expr . (ExprL . ExprR) . ExprL -> Expr . ExprL . (ExprR . ExprL)
--  joinRL :: ExprR . ExprL -> Expr
--  push :: Expr . ExprL -> Expr . ExprL . Expr
--  push = fmap (fmap joinRL) . assoc . split
--
--  pushLR :: Expr . ExprL -> Expr . ExprL . ExprR . ExprL
--  pushLR = assoc . split
--
--  part :: ExprF . AndF -> ExprF . (AndF . AndO) . AndF
--
--  dart :: (AndF . AndO) . AndF -> AndF . (AndO . AndF)
--
--  push :: ExprF
--
--  A "leafy" Subtree of And's is: AndF a -> AndF (Pure a)
--    A subexpression of Just And's
--
--  A "branchy" Subtree of And's is AndF (not Pure) -> AndF (ExprF a)
--    A subexpression of And's ending is non-And, then And, etc..
-- @
--
pushWithSplits :: ()
pushWithSplits = ()


-- | Several logical forms:
--
-- @
--  Or (And a)
--  (x && y) || z => (x || y) && (y || z)
--  (\x y z w->(x && y) || (z && w) == (x || z) && (x || w) && (y || z) && (y || w))
--
--  DNF
--  (x && y) || (z && w)
--
--  CNF
--  (w ∨ x) ∧ (w ∨ y) ∧ (x ∨ z) ∧ (y ∨ z)
--
--  (x && y) `xor` (z && w)
--
--  CNF
--  (¬w ∨ ¬x ∨ ¬y ∨ ¬z) ∧ (w ∨ x) ∧ (w ∨ y) ∧ (x ∨ z) ∧ (y ∨ z)
-- @
--
logicalForms :: ()
logicalForms = ()


-- | `undefined`
partAndo :: ExprF (AndF a) -> ExprF (AndO (AndF a))
partAndo (Pure x) = Pure (Pure x)
partAndo _ = undefined

dartAndo :: ExprF (AndO (AndF a)) -> ExprF (AndF (AndO a))
dartAndo = undefined

-- | `undefined`
--
-- @
--  dartAnd (Pure x) = _ x
--  dartAnd (Free (L1 (And x y))) = _ x
--  dartAnd (Free (R1 (L1 x))) = _ x
--  dartAnd (Free (R1 (R1 x))) = _ x
--  dartAnd (Free (R1 (R1 (L1 (Xor x y))))) = _ x y
--  dartAnd (Free (R1 (R1 (R1 (Not x))))) = _ x
--
--  dartAnd (Pure (Pure x)) = Pure $ Pure <$> x
--  dartAnd (Pure (Free (L1 (And x y)))) =
-- @
--
dartAnd :: ExprF (ExprF (AndF a)) -> ExprF (AndF (ExprF a))
dartAnd = undefined


pushAnd :: ExprF (AndF a) -> ExprF (AndF (ExprF a))
pushAnd = dartAnd . partAnd

qartAnd :: ExprF (ExprF (AndF a)) -> ExprF (AndF a)
qartAnd = join

-- `undefined`
bartAnd :: ExprF (AndF (ExprF a)) -> ExprF (ExprF (AndF a))
bartAnd = undefined

pullAnd :: ExprF (AndF (ExprF a)) -> ExprF (AndF a)
pullAnd = qartAnd . bartAnd

-- | Simplify recursively
--
-- @
--  simplAndEqLoop xs =
--    (>>= fromAndF) . pullAnd . fmap simplAndEqArgsF . pushAnd . getAndF $ xs
-- @
--
simplAndEqLoop :: Eq a => ExprF a -> ExprF a
simplAndEqLoop xs =
  (>>= fromAndF) . pullAnd . fmap simplAndEqArgsF . pushAnd . getAndF $ xs

