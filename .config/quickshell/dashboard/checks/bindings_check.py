"""Exercise panel shortcuts through a temporary Linux uinput keyboard."""
import fcntl
import json
import os
from pathlib import Path
import struct
import subprocess
import time

IPC = ['quickshell', 'ipc', '-p', str(Path(__file__).parents[2] / 'shell.qml'), 'call', 'panels']
NAME = 'qs-binding-validation'
META, SHIFT = 125, 42


def command(arguments):
    return subprocess.check_output(arguments, text=True)


def status():
    return json.loads(command(IPC + ['status']))


def until(predicate, label):
    deadline = time.monotonic() + 3
    while time.monotonic() < deadline:
        if predicate():
            print('PASS:', label, flush=True)
            return
        time.sleep(0.05)
    raise AssertionError(label)


original_workspace = json.loads(command(['hyprctl', 'activeworkspace', '-j']))['id']
assert not any(status()[key] for key in ['launcher', 'news', 'notifications', 'media', 'telegram'])
fd = os.open('/dev/uinput', os.O_WRONLY | os.O_NONBLOCK)
created = False
try:
    # ioctl numbers and structure layouts from linux/uinput.h and linux/input.h.
    fcntl.ioctl(fd, 0x40045564, 1)  # UI_SET_EVBIT, EV_KEY
    for key in range(1, 256):
        fcntl.ioctl(fd, 0x40045565, key)  # UI_SET_KEYBIT
    setup = struct.pack('HHHH80sI', 3, 1, 1, 1, NAME.encode(), 0)
    fcntl.ioctl(fd, 0x405c5503, setup)  # UI_DEV_SETUP
    fcntl.ioctl(fd, 0x5501)  # UI_DEV_CREATE
    created = True
    until(lambda: any(device['name'] == NAME for device in json.loads(command(['hyprctl', 'devices', '-j']))['keyboards']),
          'Temporary keyboard recognized')

    def chord(key, shift=False):
        keys = [META] + ([SHIFT] if shift else []) + [key]
        try:
            for code in keys:
                os.write(fd, struct.pack('llHHi', 0, 0, 1, code, 1))
                os.write(fd, struct.pack('llHHi', 0, 0, 0, 0, 0))
                time.sleep(0.01)
        finally:
            for code in reversed(keys):
                os.write(fd, struct.pack('llHHi', 0, 0, 1, code, 0))
                os.write(fd, struct.pack('llHHi', 0, 0, 0, 0, 0))
                time.sleep(0.01)

    for key, shift, field, label in [(20, True, 'telegram', 'Super+Shift+T'),
                                    (20, False, 'telegram', 'Super+T'),
                                    (19, False, 'telegram', 'Super+R'),
                                    (32, False, 'launcher', 'Super+D'),
                                    (49, True, 'news', 'Super+Shift+N'),
                                    (23, True, 'notifications', 'Super+Shift+I')]:
        chord(key, shift)
        if field == 'news' and not status().get('newsEnabled', True):
            command(IPC + ['news'])
            assert not status()['news']
            print('PASS: Super+Shift+N leaves the hidden top-bar news module closed', flush=True)
            continue
        until(lambda: status()[field], label + ' opens its panel')
        if label == 'Super+Shift+T':
            assert status()['telegramFullscreen']
        chord(key, shift)
        until(lambda: not status()[field], label + ' closes its panel')
    if status()['hasTrack']:
        chord(57, True)
        until(lambda: status()['media'], 'Super+Shift+Space opens playing media')
        chord(57, True)
        until(lambda: not status()['media'], 'Super+Shift+Space closes playing media')
    else:
        chord(57, True)
        command(IPC + ['media'])
        assert not status()['media']
        print('PASS: Media shortcut is registered and handler responds; no active track', flush=True)
finally:
    if created:
        fcntl.ioctl(fd, 0x5502)  # UI_DEV_DESTROY
    os.close(fd)
    for field, handler in [('telegram', 'telegramFullscreen'), ('launcher', 'launcher'),
                           ('news', 'news'), ('notifications', 'notifications'), ('media', 'media')]:
        state = status()
        if state[field]:
            if field == 'telegram' and not state['telegramFullscreen']:
                handler = 'telegram'
            command(IPC + [handler])
    command(['hyprctl', 'dispatch', 'hl.dsp.focus({ workspace = ' + str(original_workspace) + ' })'])
