#!/bin/sh

nitrogen --restore
discord --start-minimized &
polybar -c ~/.config/polybar/config2.ini computer &
