{
  description = "ITEA";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/master";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        config = {
          packageOverrides = pkgs: {
            haskell = pkgs.haskell // {
              packages = pkgs.haskell.packages // {
                ghc = pkgs.haskell.packages.ghc8107.override {
                  overrides = self: super: {
                    mltool = pkgs.haskell.lib.dontCheck super.mltool;
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
