{ cabal, contravariant, profunctors }:

cabal.mkDerivation (self: {
  pname = "product-profunctors";
  version = "0.4.1";
  src = ./.;
  buildDepends = [ contravariant profunctors ];
  doCheck = false;
  meta = {
    description = "product-profunctors";
    license = "unknown";
    platforms = self.ghc.meta.platforms;
  };
})
