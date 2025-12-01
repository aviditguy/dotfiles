#!/usr/bin/env bash
# cliplink-x11.sh – log links from clipboard to file
OUTFILE="$PWD/links.txt"
POLL=1   # check every 1 second

while true; do
    CLIP=$(xclip -selection clipboard -o 2>/dev/null)
    if [[ "$CLIP" =~ ^https?:// ]]; then
        if ! grep -qxF "$CLIP" "$OUTFILE" 2>/dev/null; then
            echo "$CLIP" >> "$OUTFILE"
            echo "Saved: $CLIP"
        fi
    fi
    sleep $POLL
done
