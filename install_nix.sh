NIXPKGS_ALLOW_BROKEN=1 nix build --impure
cp ./result/bin/itea ./python/
cd python 
pip install .
