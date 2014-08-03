#!/bin/bash
set -e

# synchronize build results to a remote server for deployment
r.js -o app.build.js
rsync -rtvz .cabal-sandbox/bin prod.cfg snaplets static build-js $1
