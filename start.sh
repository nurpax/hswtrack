#!/bin/bash

./.cabal-sandbox/bin/hswtrack &

while inotifywait -r -e modify static snaplets; do
    echo Reload templates
    curl http://localhost:8000/heistReload
done
