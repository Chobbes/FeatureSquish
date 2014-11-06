{ pkgs ? (import <nixpkgs> {})
, haskellPackages ? pkgs.haskellPackages_ghc783
}:

haskellPackages.cabal.mkDerivation (self: {
  pname = "FeatureSquish";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = with haskellPackages; [ split random ];
  buildTools = with haskellPackages; [ cabalInstall ];
  meta = {
    description = "Time the speed of a serial connection with an Arduino.";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
