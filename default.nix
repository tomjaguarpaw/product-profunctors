{ cabal
, contravariant
, profunctors
}:
cabal.mkDerivation (self: {
  pname = "product-profunctors";
  version = "0.6.2";
  src = ./. ;
  buildDepends = [ contravariant profunctors ];
  testDepends = [ profunctors ];
  meta = {
    homepage = "https://github.com/tomjaguarpaw/product-profunctors";
    description = "product-profunctors";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
