#!/bin/sh

# Launch background services and desktop
# sleep 2
# polybar -c ~/.config/polybar/config.ini computer-bar1 &
# polybar -c ~/.config/polybar/config.ini computer-bar2 &
# polybar -c ~/.config/polybar/config.ini computer-bar3 &
# polybar -c ~/.config/polybar/config.ini computer-bar4 &
nitrogen --restore
# picom &
# systemctl --user start ulauncher.service
discord --start-minimized &
polybar computer &

# Launch applications

#i3-msg workspace 9
#sleep 0.5
#alacritty --hold -e screenfetch &
#sleep 0.2
#i3-msg floating toggle
#sleep 0.2
#i3-msg move right 520 px
#i3-msg move up 180 px
#i3-msg resize shrink height 200 px
#i3-msg workspace 2

#i3-msg workspace 9
#sleep 1
#spotify &
#sleep 1
#i3-msg workspace 2
#sleep 1
#alacritty --hold -e htop &
#sleep 1
#alacritty --hold -e cava &
#sleep 1
#i3-msg split v
#sleep 1
#alacritty --hold -e swaglyrics -c &
#sleep 1
#i3-msg resize shrink width 7 px or 7 ppt
#i3-msg split h
#sleep 1
#alacritty --hold -e alsamixer &
#sleep 1
#i3-msg resize grow height 9 px or 9 ppt

# Stocks Launch
#i3-msg workspace 1
#sleep 0.5
#sh /home/prayuj/projects/finances/sync.sh
#alacritty --hold -e ticker --config /home/prayuj/.ticker.yaml &
#sleep 0.25
#i3-msg split v
#sleep 0.25
#alacritty --hold -e tickrs -c candle -p --hide-help --hide-toggle --trunc-pre -s SPY,VTI,TSLA,AAPL,GOOGL,AMZN,FCEL,NIO &
#sleep 0.25
#i3-msg resize grow height 20 px or 20 ppt
