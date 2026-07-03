apt-get install build-essential -y
apt-get install curl -y
apt-get install libffi-dev -y
apt-get install libffi8 -y
apt-get install libgmp-dev -y
apt-get install libgmp10 -y
apt-get install libncurses-dev -y
apt-get install pkg-config -y
BOOTSTRAP_HASKELL_NONINTERACTIVE=true
BOOTSTRAP_HASKELL_GHC_VERSION=9.10.1
BOOTSTRAP_HASKELL_CABAL_VERSION=3.12.1.0
BOOTSTRAP_HASKELL_INSTALL_NO_STACK=true
BOOTSTRAP_HASKELL_INSTALL_HLS=false
./bootstrap-haskell
. /root/.ghcup/env
cabal build "exe:ParityGames"