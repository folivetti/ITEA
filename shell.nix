let
  pkgs_stable = import <nixos> { };
  pkgs = import <nixos-unstable> { };
  eli5 = import ./eli5.nix {
    lib = pkgs.gcc10Stdenv.lib;
    buildPythonPackage = pkgs.python38Packages.buildPythonPackage;
    fetchPypi = pkgs.python38Packages.fetchPypi;
    pythonPackages = pkgs.python38Packages;
  };
  pmlb = import ./pmlb.nix {
    lib = pkgs.gcc10Stdenv.lib;
    buildPythonPackage = pkgs.python38Packages.buildPythonPackage;
    fetchPypi = pkgs.python38Packages.fetchPypi;
    pythonPackages = pkgs.python38Packages;
  };
in
  pkgs.stdenv.mkDerivation {
    name = "itea-env";
    hardeningDisable = [ "all" ]; 

    buildInputs = with pkgs; [
        # python environment for bindings and scripting
        (pkgs.python38.withPackages (ps: with ps; [ numpy scikitlearn pandas]))
        pkg-config
        stack
        ghc
        eli5
        pmlb
      ];
    }
