#!/bin/bash

(cd ../publish && python2 -m SimpleHTTPServer) &

while true; do
    make examples
    echo "Watching for changes..."
    inotifywait -qre close_write .
done
