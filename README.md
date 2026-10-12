# Screenshots #
![Screenshot](.screenshots/screenshot.png)

# Screencast #
https://github.com/user-attachments/assets/4155bb8f-b879-4eb7-b076-4755835c8ffb

# Desktop configuration #

Hyprland uses `.config/hypr/hyprland.lua` and Quickshell's `panels` IPC for the
launcher, Telegram, notifications, and media shortcuts. Super+Shift+W opens
the dashboard; Super+Shift+O opens the workspace world. The SDDM theme and its
public news/market feed collector are in `.config/sddm/neon-dusk/`; the dashboard
reads their cache from `/var/cache/sddm-news/`.

The audio switcher and its user service/timer are included alongside the
Alacritty, Spaceship prompt, wallpaper, and Waybar helper files. Installing or
enabling the SDDM system files is a separate administrative step.

# Private configuration #

Keep credentials in the ignored `.secrets` and `.config/quickshell/.env` files
with permissions `600`. The legacy Waybar news helper reads `NYT_API_KEY` from
the environment; build it with `make -C ~/.config/waybar/scripts/news build`.
The previously committed NYT key must be rotated; it remains in older history.

Telegram requires a local `.config/quickshell/.venv-tg/` with the dependencies
in `.config/quickshell/scripts/telegram-requirements.txt`, plus `API_ID`,
`API_HASH`, and `TG_RAY` in Quickshell's `.env`. Run `telegram-auth` once to
create the ignored Telegram session.

This repository lives in the home directory. `.gitignore` includes only the
desktop source folders and excludes unrelated application state, projects,
credentials, dependencies, and build output. Extend its allowlist when adding
a new configuration folder.
