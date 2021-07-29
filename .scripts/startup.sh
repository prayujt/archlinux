#!/bin/sh

nitrogen --restore
discord --start-minimized &
polybar -c ~/.config/polybar/config.ini monitor &
polybar -c ~/.config/polybar/config.ini computer &
