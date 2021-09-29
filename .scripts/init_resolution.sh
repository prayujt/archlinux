#!/bin/sh

isHDMIConnected() { 
	local xRandr=$(xrandr -q)
	[ "$xRandr" == "${xRandr#*DP-3 con}" ] || return 0
	return 1
}

if isHDMIConnected
then
	xrandr --output DP-3 --primary
	xrandr --output eDP-1 --mode 1920x1080
	i3-msg workspace 1 output DP-3
	i3-msg workspace 2 output eDP-1
	sleep 2
	sh ~/.scripts/startup.sh
else
	xrandr --output eDP-1 --primary
	xrandr --output eDP-1 --mode 1920x1080
	sleep 2
	sh ~/.scripts/startup2.sh
fi
