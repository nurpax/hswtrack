#!/bin/bash

# synchronize build results to a remote server for deployment

rsync -rtvz .cabal-sandbox/bin snaplets static $1
