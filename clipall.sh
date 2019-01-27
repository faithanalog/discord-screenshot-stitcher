#!/bin/sh
find screenshots -type f -print0 \
    | sort -z \
    | xargs -0 ./discordclip ot
echo "Done"
