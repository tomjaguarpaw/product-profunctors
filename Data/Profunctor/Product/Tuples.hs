module Data.Profunctor.Product.Tuples where

type T0 = ()
type T1 a = a
type T2 a b = (a, T1 b)
type T3 a b c = (a, T2 b c)
type T4 a b c d = (a, T3 b c d)
type T5 a b c d e = (a, T4 b c d e)
type T6 a b c d e f = (a, T5 b c d e f)
type T7 a b c d e f g = (a, T6 b c d e f g)
type T8 a b c d e f g h = (a, T7 b c d e f g h)
type T9 a b c d e f g h a9 = (a, T8 b c d e f g h a9)
type T10 a b c d e f g h a9 a10 = (a, T9 b c d e f g h a9 a10)
type T11 a b c d e f g h a9 a10 a11 = (a, T10 b c d e f g h a9 a10 a11)
type T12 a b c d e f g h a9 a10 a11 a12 = (a, T11 b c d e f g h a9 a10 a11 a12)
type T13 a b c d e f g h a9 a10 a11 a12 a13 =
  (a, T12 b c d e f g h a9 a10 a11 a12 a13)
type T14 a b c d e f g h a9 a10 a11 a12 a13 a14 =
  (a, T13 b c d e f g h a9 a10 a11 a12 a13 a14)
type T15 a b c d e f g h a9 a10 a11 a12 a13 a14 a15 =
  (a, T14 b c d e f g h a9 a10 a11 a12 a13 a14 a15)
type T16 a b c d e f g h a9 a10 a11 a12 a13 a14 a15 a16 =
  (a, T15 b c d e f g h a9 a10 a11 a12 a13 a14 a15 a16)
type T17 a b c d e f g h a9 a10 a11 a12 a13 a14 a15 a16 a17 =
  (a, T16 b c d e f g h a9 a10 a11 a12 a13 a14 a15 a16 a17)
type T18 a b c d e f g h a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 =
  (a, T17 b c d e f g h a9 a10 a11 a12 a13 a14 a15 a16 a17 a18)
