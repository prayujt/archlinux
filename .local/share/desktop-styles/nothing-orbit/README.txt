NOTHING / ORBIT — a reversible desktop experiment

The active setup combines Nothing's black/ivory/red palette and dotted typography
with an original ASCII planet-and-rings scene inspired by Orbiter. It runs on the
existing Hyprland + Quickshell installation. Existing keybindings, launcher,
notifications, Telegram and Sonos backends remain in place. The terminal keeps
its blur, padding controls, keyboard bindings and font, with a neutral palette.

SWITCHING
  desktop-style original     restore the saved glass / forest desktop
  desktop-style nothing      re-enable this experiment
  desktop-style status       show the selected profile

While the new style is active:
  Super + Alt + Backspace restores the original.

The restore path was tested live. All four original configuration files matched
byte-for-byte after restoring; the previous aurora video resumed; both experimental
desktop surfaces disappeared. Nothing / Orbit was then re-enabled successfully.

SNAPSHOT
  ~/.local/state/desktop-styles/original/config/
  ~/.local/state/desktop-styles/original/wallpaper/

The snapshot includes the exact configs at the start of this experiment, the
active aurora video, its native mpvpaper binaries and its launch arguments. It
survives reboots and does not rely on /tmp. Shell startup and Spaceship settings
were not changed. Profile switching copies saved visual configuration snapshots;
manual changes to those four live files should be saved into the corresponding
profile before switching if they are to be preserved.

FILES
  ~/.local/bin/desktop-style                  profile switch / restore controller
  ~/.config/quickshell/OrbitalDesktop.qml     scene and live widgets
  ~/.local/share/desktop-styles/nothing-orbit/config/  experimental visual configs
  ~/.local/share/desktop-styles/nothing-orbit/build-style.py  reproducible palette edits
  ~/.local/share/desktop-styles/nothing-orbit/metrics.py  live CPU/RAM/uptime source
  desktop-preview.png                        1920x1080 desktop capture
  desktop-preview-1440p.png                   2560x1440 desktop capture
  launcher-preview.png                       launcher on the live desktop

VALIDATION
  QML lint, Hyprland Lua syntax and Alacritty TOML parsing passed.
  Hyprland reports no configuration errors.
  Both monitor variants rendered and were visually inspected.
  The launcher opened correctly and was closed again after the check.
  The scene has no continuous animation/render loop. CPU/RAM/uptime readings
  update every three seconds; the clock updates normally.

SCOPE / FURTHER WORK
  The ASCII scene is static. Orbiter's mouse-following shader is a separate
  Plasma-specific implementation. A comparable live version on this desktop
  would require a custom Qt/Wayland shader and pointer handling.
  A closer full Glyph experience could add a vertical sidebar and edge-light
  OSD, then separate lock/login theming. This version retains the existing bar
  and working controls. The login screen and authentication were not changed.

REFERENCES
  Nothing OS post:
  https://www.reddit.com/r/unixporn/comments/1xeuuwb/hyprland_nothing_os_rice/
  Glyph source/reference screenshots:
  https://github.com/Nebelung-cloud/glyph-rice
  Orbiter's author description:
  https://www.reddit.com/r/unixporn/comments/1xf8z7s/kde_plasma_orbiter_my_ascii_space_rice_with_its/
  Doto font (SIL Open Font License, retained in assets/Doto-OFL.txt):
  https://github.com/google/fonts/tree/main/ofl/doto

The new scene and widgets were written locally; no third-party installer was run.
