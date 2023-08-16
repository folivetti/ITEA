# !/bin/bash

# install ghcup
export BOOTSTRAP_HASKELL_NONINTERACTIVE=1
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | bash
export PATH=$PATH:~/.ghcup/bin:~/.cabal/bin 

#conda activate srbench
cabal install
cp ~/.cabal/bin/itea ./python/
cd python 
pip install .
