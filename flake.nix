{
  description = "Extensible records for Haskell.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    nur.url = "github:nix-community/nur";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, nur, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system: let
      nurpkgs = import nur {
          nurpkgs = nixpkgs.legacyPackages.x86_64-linux;
          pkgs = nixpkgs.legacyPackages.x86_64-linux;
        };
      compiler = "8107";
      # compiler = "921";
      pkgs = import nixpkgs {
        inherit system;
      };
      dontCheck = pkgs.haskell.lib.dontCheck;
      dontHaddock = pkgs.haskell.lib.dontHaddock;
      overrideSrc = pkgs.haskell.lib.overrideSrc;
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
            lens-aeson = overrideSrc super.lens-aeson {
              version = "1.2";
              src = pkgs.fetchFromGitHub {
                owner = "lens";
                repo = "lens-aeson";
                rev = "28ff4ffd778c6e42084cff26e9c9d973b0645cd9";
                hash = "sha256-AO7X3fvdgTYJeoVViQSXZq3BqLtHl8/PAcSoxWxfpXg=";
              };
            };
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
        hspkgs.haskell-language-server
        hspkgs.cabal-plan
        nurpkgs.repos.amesgen.cabal-docspec
      ];
    };
  });
}
