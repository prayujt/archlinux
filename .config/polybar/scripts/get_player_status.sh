#!/bin/bash

PARENT_BAR="computer"
PARENT_BAR_PID=$(pgrep -a "polybar" | grep "$PARENT_BAR" | cut -d" " -f1)

FORMAT="{{ title }} - {{ artist }}"

update_hooks() {
    while IFS= read -r id
    do
        polybar-msg -p "$id" hook spotify-play-pause $2 1>/dev/null 2>&1
    done < <(echo "$1")
}

#PLAYERS=$(playerctl -al 2>/dev/null | wc -w)
#if [ $PLAYERS = 2 ]; then
#    SPOTIFY_STATUS=$(playerctl --player=spotify status 2>/dev/null)
#    CHROME_STATUS=$(playerctl --player=chromium status 2>/dev/null)

#    if [ "$SPOTIFY_STATUS" = "Playing"  ] && [ "$CHROME_STATUS" != "Playing"    ]; then
#        PLAYER="spotify"
#    elif [ "$SPOTIFY_STATUS" != "Playing"  ] && [ "$CHROME_STATUS" = "Playing"    ]; then
#        PLAYER="chromium"
#    elif [ "$SPOTIFY_STATUS" = "Paused"  ] && [ "$CHROME_STATUS" = "Paused"    ]; then
#        PLAYER="chromium"
#    fi
#fi
#else
#    PLAYER=$(playerctl -l)
#fi

PLAYERCTL_STATUS=$(playerctl status 2>/dev/null)
EXIT_CODE=$?

if [ $EXIT_CODE -eq 0 ]; then
    STATUS=$PLAYERCTL_STATUS
else
    STATUS="No music is playing"
fi

if [ "$1" == "--status" ]; then
    echo "$STATUS"
else
    if [ "$STATUS" = "Stopped" ]; then
        echo "No music is playing"
    elif [ "$STATUS" = "Paused"  ]; then
        update_hooks "$PARENT_BAR_PID" 1
        playerctl metadata --format "$FORMAT"
    elif [ "$STATUS" = "No music is playing"  ]; then
        echo "$STATUS"
    else
        update_hooks "$PARENT_BAR_PID" 2
        playerctl metadata --format "$FORMAT"
    fi
fi
