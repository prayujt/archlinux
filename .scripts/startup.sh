#!/bin/sh

nitrogen --restore
polybar -c ~/.config/polybar/config.ini monitor &
sleep 0.5
polybar -c ~/.config/polybar/config.ini computer &
sleep 0.5
python ~/.scripts/pulseaudio-watcher.py | xob -s monitor &
discord --start-minimized &
