{
  description = "ITEA";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/master";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        myintervals = pkgs.fetchFromGitHub {
          owner = "folivetti";
          repo  = "intervals";
          rev = "1c6261bcc8c8d36cdb3678379ab01bbbd0af7364";
          sha256 = "sha256-hJqLZS1YwAGddIHTrmb2V2rtCKuulgqxBHcCTacPLVg=";
        };
        config = {
          packageOverrides = pkgs: {
            haskell = pkgs.haskell // {
              packages = pkgs.haskell.packages // {
                ghc = pkgs.haskell.packages.ghc8107.override {
                  overrides = self: super: {
                    mltool = pkgs.haskell.lib.dontCheck super.mltool;
                    intervals = super.callCabal2nix "intervals" myintervals {};
                  };
                };
              };
            };
          };
        };

        pkgs = import nixpkgs {
          inherit system config;
        };

        drv = pkgs.haskell.packages.ghc.callCabal2nixWithOptions "ITEA" ./. "--no-check" {};
      in {
        defaultPackage = drv;
        devShell = drv.env;
      });
}
