So we have a class

```
Functor :: (Type -> Type) -> Constraint
```

It grants us the ability to "map" over the type index.

```
fmap :: (f : Type -> Type)
     -> (a : Type) -> (a' : Type)
     -> Functor f
     -> (a -> a') -> f a -> f a'
```

We also have

```
Bifunctor :: (Type -> Type -> Type) -> Constraint
```

which grants us the ability to "map" over *two* type indices.

```
bimap :: (f : Type -> Type -> Type)
      -> (a : Type) -> (a' : Type)
      -> (b : Type) -> (b' : Type)
      -> Bifunctor f
      -> (a -> a') -> (b -> b') -> f a b -> f a' b'
```

We can also imagine `Trifunctor`, `Quadrafunctor`, .... Can we
generalise?  We hypothesise a class

```
Multifunctor :: ((n : Nat) -> Vec n Type -> Type) -> Constraint
```

granting us the ability to "map" over `n` type indices.  What do we
map?  We map `Multifunction`s.

```
-- Multifunction 3 [a, b, c] [a', b', c'] = (a -> a', (b -> b', (c -> c', ())))

Multifunction :: (n : Nat) -> Vec n Type -> Vec n Type -> Type
Multifunction = \(n : Nat) (a : Vec n Type) (a' : Vec n Type) -> case n of
  Succ nm1 -> case (a, a') of (a0 :: arest, a'0 :: a'rest)
    -> (a0 -> a'0, F nm1 arest a'rest)
  Zero -> case (a, a') of (Nil, Nil) -- don't need case but let's do it anyway
    -> ()

multimap :: (n : Nat)
         -> (f : Vec n Type -> Type)
         -> (a : Vec n Type) -> (a' : Vec n Type)
         -> Multifunctor n f
         -> Multifunction n a a' -> f a -> f a'
```
