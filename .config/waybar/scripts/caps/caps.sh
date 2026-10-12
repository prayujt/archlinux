#!/usr/bin/env bash
while true; do
  status="OFF"

  if command -v evtest >/dev/null 2>&1; then
    for dev in /dev/input/by-path/*-event-kbd; do
      [ -e "$dev" ] || continue
      if evtest --query "$dev" EV_LED LED_CAPSL 2>/dev/null | grep -q 1; then
        status="ON"
        break
      fi
    done
  fi

  if [ "$status" = "OFF" ]; then
    if grep -q 1 /sys/class/leds/*[Cc]apslock*/brightness 2>/dev/null; then
      status="ON"
    fi
  fi

  if [ "$status" = "ON" ]; then
    echo '{"text":" CAPS","class":"caps-on"}'
  else
    echo '{}'
  fi

  sleep 0.1
done
