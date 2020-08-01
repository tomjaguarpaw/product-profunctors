# 0.11.0.0

* Added `makeAdaptorAndInstanceInferrable` which has better inference
  properties, at the cost having to turn on `UndecidableInstances`.
  The tuple instances are now made by this method too.

# 0.10.0.1

* Added `Data.Profunctor.Product.Examples`

# 0.10.0.0

* Removed `ProductContravariant`, `AndArrow`, `defaultContravariantProduct`,
  `PPOfContravariant`, `unPPOfContravariant`, `cdef`

* Deprecated `defaultEmpty`, `defaultProfunctorProduct`, `defaultPoint`

# 0.9.0.0

* Added more `ProductProfunctor/SumProfunctor` instances including for
  types in `Data.Bifunctor`

* Added `Data.Profunctor.Product.Adaptor` which derives
  `genericAdaptor` with `Generic`

* Added ability to derive `Default` instance with `Generic`

Sorry, we didn't track changes before version 0.9.0.0
