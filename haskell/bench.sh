#!/usr/bin/env bash

echo "running: cabal new-run bench -- -m prefix $@"
cabal new-run bench -- -m prefix "$@"
