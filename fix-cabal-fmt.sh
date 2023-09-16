#! /bin/bash

fdfind --extension cabal --exclude 'dist-newstyle/*' --exclude 'dist/*' --exclude '.stack-work/*' --exec bash -c "~/.cabal/bin/cabal-fmt -i {} || true"