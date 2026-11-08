#!/usr/bin/env python3
"""Build the Midnight Meadow variant from the untouched glass desktop backup."""
import json
import re
import shutil
from pathlib import Path

home = Path.home()
profile = Path(__file__).resolve().parent
source = home / '.local/state/desktop-styles/original/config'
colors = dict(bgColor='#d9081719', textColor='#e8edd4', subtext='#a0b2a0',
              accent='#abc786', clockColor='#dce5a4', volumeColor='#d8ce99',
              mutedColor='#d18b7a', gainColor='#a1c99b', surface='#1d342d',
              inkColor='#111d18', outline='#557f9986')

def write(name, text):
    path = profile / 'config' / name
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text)

shell = (source / 'quickshell/shell.qml').read_text()
shell = shell.replace('readonly property string fontFamily: "GeistMono Nerd Font Mono"', 'readonly property string fontFamily: "Adwaita Sans"')
shell = shell.replace('// -- colors (neon current) --', '// Midnight Meadow: soft highlights over deep green glass.')
for key, value in colors.items():
    shell, count = re.subn(r'(?m)^(    readonly property color ' + key + r': ).*$',
                          lambda match: match[1] + json.dumps(value), shell)
    assert count == 1, key
shell = shell.replace('return JSON.stringify({launcher:', 'return JSON.stringify({desktopStyle: "meadow", launcher:', 1)
needle = '                // -- clock face --'
assert shell.count(needle) == 1
shell = shell.replace(needle, '''                MouseArea {
                    anchors.fill: parent
                    enabled: root.pillMode === "clock"
                    acceptedButtons: Qt.LeftButton
                    property int taps: 0
                    property double lastTap: 0
                    onPressed: {
                        const time = Date.now();
                        taps = time - lastTap < 350 ? taps + 1 : 1;
                        lastTap = time;
                        if (taps === 5) {
                            taps = 0;
                            Quickshell.execDetached(["/home/prayuj/.local/bin/desktop-style", "toggle"]);
                        }
                    }
                }

''' + needle)
write('quickshell/shell.qml', shell)

hypr = (source / 'hypr/hyprland.lua').read_text()
hypr, count = re.subn(r'(?m)^(\s*active_border\s*=).*$', r'\1 { colors = {"rgba(b6cc9299)", "rgba(dce5a466)"}, angle = 45 },', hypr)
assert count == 1
hypr, count = re.subn(r'(?m)^(\s*inactive_border\s*=).*$', r'\1 "rgba(7f998633)",', hypr)
assert count == 1
hypr += '\nhl.bind("SUPER + CTRL + ALT + M", hl.dsp.exec_cmd("/home/prayuj/.local/bin/desktop-style toggle"))\n'
write('hypr/hyprland.lua', hypr)
write('alacritty/alacritty.toml', (source / 'alacritty/alacritty.toml').read_text())

assets = profile / 'assets'
assets.mkdir(exist_ok=True)
video = assets / 'midnight-meadow.mp4'
if not video.exists():
    shutil.copy2('/tmp/nature-wallpapers-2026-11-08/motion/midnight-meadow.mp4', video)
write('hypr/hyprpaper.conf', '\n'.join('wallpaper {\n  monitor = ' + output + '\n  path = ' + str(assets / 'midnight-meadow.jpg') + '\n  fit_mode = cover\n}\n' for output in ['DP-3', 'HDMI-A-1']) + '\nsplash = false\n')
command = [str(home / '.local/state/desktop-styles/original/wallpaper/bin/mpvpaper'),
           '-l', 'bottom', '-p', '-o',
           'no-audio terminal=no msg-level=all=warn loop-file=inf hwdec=auto-safe panscan=1 keepaspect=yes video-sync=display-resample',
           '*', str(video)]
(profile / 'state.json').write_text(json.dumps(dict(wallpaper_active=True, wallpaper_command=command), indent=2) + '\n')
print('Midnight Meadow glass profile built; the original backup is unchanged.')
