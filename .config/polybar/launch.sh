#!/bin/bash

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

(sleep 2; polybar -c ~/.config/polybar/config.ini -r base) &
(sleep 3; polybar -c ~/.config/polybar/config.ini -r background) &
echo "Polybar launched..."
