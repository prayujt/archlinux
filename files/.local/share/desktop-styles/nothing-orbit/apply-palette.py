#!/usr/bin/env python3
"""Apply a monochrome palette without replacing unrelated settings."""
import fcntl, json, os, re, stat, sys, tempfile
from pathlib import Path

home = Path.home()
profile = home / '.local/share/desktop-styles/nothing-orbit'


def replace_file(path, text):
    path = path.resolve()
    if path.read_text() == text:
        return
    mode = stat.S_IMODE(path.stat().st_mode)
    fd, temporary = tempfile.mkstemp(prefix='.palette-', dir=path.parent)
    with os.fdopen(fd, 'w') as stream:
        stream.write(text)
    os.chmod(temporary, mode)
    os.replace(temporary, path)


def apply(mode, live=True):
    colors = json.loads((profile / 'palettes.json').read_text())[mode]
    bases = [home / '.config', profile / 'config'] if live else [profile / 'config']
    for base in bases:
        path = base / 'alacritty/alacritty.toml'
        section = None
        output = []
        for line in path.read_text().splitlines(True):
            match = re.match(r'^\[colors\.(\w+)\]', line)
            if match:
                section = match[1]
            elif line.startswith('['):
                section = None
            if section in colors['terminal']:
                match = re.match(r'^(\w+)\s*=', line)
                if match and match[1] in colors['terminal'][section]:
                    line = f'{match[1]} = "{colors["terminal"][section][match[1]]}"\n'
            output.append(line)
        replace_file(path, ''.join(output))
        path = base / 'hypr/hyprland.lua'
        text = path.read_text()
        for key, color in [('active_border', colors['activeBorder']), ('inactive_border', colors['inactiveBorder'])]:
            text = re.sub(r'(?m)^(\s*'+key+r'\s*=\s*)[^\n]+', lambda m: m[1]+'"rgba('+color+')",', text)
        replace_file(path, text)
    state = home / '.local/state/desktop-styles/appearance.json'
    data = json.dumps({'mode': mode}) + '\n'
    if live:
        replace_file(state, data)


if __name__ == '__main__':
    if len(sys.argv) != 2 or sys.argv[1] not in ['dark', 'light', 'black']:
        sys.exit('Choose dark, light or black.')
    state_dir = home / '.local/state/desktop-styles'
    with (state_dir / 'switch.lock').open('a') as lock:
        fcntl.flock(lock, fcntl.LOCK_EX)
        selected = (state_dir / 'current').read_text().strip() if (state_dir / 'current').exists() else 'nothing'
        if selected == 'nothing':
            apply(sys.argv[1])
