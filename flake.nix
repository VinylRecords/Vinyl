{
  description = "Extensible records for Haskell.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system: let

      compiler = "8107";
      # compiler = "921";
      pkgs = import nixpkgs {
        inherit system;
      };
      dontCheck = pkgs.haskell.lib.dontCheck;
      dontHaddock = pkgs.haskell.lib.dontHaddock;
      hspkgs = (pkgs.haskell.packages."ghc${compiler}").override {
        overrides = self: super: 
          if compiler == "921"
          then { 
            QuickCheck = dontCheck super.QuickCheck;
            SHA = super.SHA.override { QuickCheck = self.QuickCheck; };
            optparse-applicative = dontCheck super.optparse-applicative;
            case-insensitive = dontCheck super.case-insensitive;
            base-compat-batteries = dontCheck super.base-compat-batteries;
            attoparsec = dontCheck super.attoparsec;
            generic-deriving = dontHaddock super.generic-deriving;
            tasty-golden = dontHaddock super.tasty-golden;
            aeson = dontCheck (super.callHackage "aeson" "2.0.3.0" {});
            microlens = pkgs.haskell.lib.dontHaddock super.microlens;
          } 
          else { };
      };
      drv = hspkgs.callPackage ./default.nix {};
      ghc = hspkgs.ghc.withPackages (ps: 
        drv.passthru.getBuildInputs.haskellBuildInputs
      );
  in {
    devShell = pkgs.mkShell {
      buildInputs = [
        ghc
        hspkgs.cabal-install
      ];
    };
  });
}
