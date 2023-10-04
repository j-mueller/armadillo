#! /bin/bash

cabal install -j armadillo-cli --overwrite-policy=always
cabal run armadillo-cli-test -- -p AMM