#!/bin/sh

nitrogen --restore
polybar -c ~/.config/polybar/config2.ini computer &
python ~/.scripts/pulseaudio-watcher.py | xob -s computer &
discord --start-minimized &
