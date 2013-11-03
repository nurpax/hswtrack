#!/bin/bash

./.cabal-sandbox/bin/hswtrack --access-log '-' --error-log '-' &

while inotifywait -r -e modify static snaplets; do
    echo Reload templates
    curl http://localhost:8000/heistReload
done
