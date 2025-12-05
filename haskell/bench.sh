#!/usr/bin/env bash

echo "running: cabal new-run bench -- -m prefix $1"
cabal new-run bench -- -m prefix $1
