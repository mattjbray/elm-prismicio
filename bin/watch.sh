#!/bin/bash

(make serve) &

fswatch -o . | while read num; do
    make
    echo "Watching for changes..."
    # inotifywait -qre close_write .
done
