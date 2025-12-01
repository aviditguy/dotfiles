#!/bin/bash

# Get battery info
info=$(acpi -b 2>/dev/null)
percent=$(echo "$info" | grep -oP '\d+(?=%)')
state=$(echo "$info" | grep -oP 'Charging|Discharging|Full')

# Default color
color="#[fg=red]"

# Use green if charging or full
if [[ "$state" == "Charging" || "$state" == "Full" ]]; then
    color="#[fg=green]"
fi

# Output for tmux status line
echo "${color}Battery: ${percent}%#[default]"
