#!/bin/bash

# Kill any existing xwinwrap or mpv
pkill xwinwrap
pkill mpv

# Wait a moment to clean up
sleep 0.5

# Pick a random video from the folder
video=$(find ~/Media/Wallpapers/Videos -type f \( -iname "*.mp4" -o -iname "*.mkv" -o -iname "*.webm" \) | shuf -n 1)

# Start wallpaper
[ -n "$video" ] && xwinwrap -ov -fs -ni -nf -- mpv --no-audio --loop --wid=%WID "$video" &

