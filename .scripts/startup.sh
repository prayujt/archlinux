#!/bin/sh

nitrogen --restore
polybar -c ~/.config/polybar/config.ini monitor &
polybar -c ~/.config/polybar/config.ini computer &
python ~/.scripts/pulseaudio-watcher.py | xob -s monitor &
discord --start-minimized &
