{ mkDerivation
, contravariant
, profunctors
, stdenv
}:
mkDerivation {
  pname = "product-profunctors";
  version = "0.6.2";
  src = ./. ;
  buildDepends = [ contravariant profunctors ];
  testDepends = [ profunctors ];
  homepage = "https://github.com/tomjaguarpaw/product-profunctors";
  description = "product-profunctors";
  license = stdenv.lib.licenses.bsd3;
}
