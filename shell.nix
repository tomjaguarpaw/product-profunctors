let hp = (import <nixpkgs> {}).haskellPackages ;
 in hp.callPackage ./. {}
