{ mkDerivation, array, base, criterion, doctest, ghc-prim, hspec
, lens, linear, microlens, mwc-random, primitive
, should-not-typecheck, singletons, stdenv, tagged, vector
, aeson, text, mtl, unordered-containers, lens-aeson
}:
let whitelistSource = src: allowedPrefixes:
      builtins.filterSource
        (path: type:
          stdenv.lib.any (allowedPrefix: stdenv.lib.hasPrefix (toString (src + "/${allowedPrefix}")) path)
                  allowedPrefixes)
                  src;
in mkDerivation {
  pname = "vinyl";
  version = "0.13.0";
  src = whitelistSource ./. [ "Data" "vinyl.cabal" "LICENSE" "Setup.hs" "test" "benchmarks" ];
  libraryHaskellDepends = [ array base ghc-prim ];
  testHaskellDepends = [
    base doctest hspec lens microlens should-not-typecheck singletons
    aeson text mtl unordered-containers lens-aeson
  ];
  benchmarkHaskellDepends = [
    base criterion linear microlens mwc-random primitive tagged vector
  ];
  doBenchmark = true;
  description = "Extensible Records";
  license = stdenv.lib.licenses.mit;
}
