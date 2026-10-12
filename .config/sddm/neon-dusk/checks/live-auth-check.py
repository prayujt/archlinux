#!/usr/bin/python
"""Exercise one rejected password through real SDDM, then restore the desktop."""
import os
import re
import secrets
import subprocess
import time
from pathlib import Path


def run(*args, **kwargs):
    return subprocess.run(args, check=True, text=True, capture_output=True, **kwargs).stdout


baseline = run("faillock", "--user", "prayuj")
if len(baseline.strip().splitlines()) > 2:
    raise SystemExit("Existing login failures: leaving them untouched and skipping the live auth test.")
submitted = False
try:
    run("systemctl", "start", "sddm.service")
    invocation = run("systemctl", "show", "sddm.service", "-p", "InvocationID", "--value").strip()
    deadline = time.monotonic() + 20
    while time.monotonic() < deadline:
        log = run("journalctl", "-b", "_SYSTEMD_INVOCATION_ID=" + invocation, "--no-pager")
        auth = re.findall(r'Xauthority path: "([^"]+)"', log)
        socket = re.findall(r'/tmp/sddm-(:\d+)-', log)
        if auth and socket and "Greeter session started successfully" in log:
            env = dict(os.environ, DISPLAY=socket[-1], XAUTHORITY=auth[-1])
            try:
                tree = run("xwininfo", "-root", "-tree", env=env)
            except subprocess.CalledProcessError:
                time.sleep(0.2)
                continue
            windows = re.findall(r'(0x[0-9a-f]+).*1920x1080\+0\+0', tree)
            if windows:
                break
        time.sleep(0.2)
    else:
        raise RuntimeError("The live greeter did not become ready")
    time.sleep(0.7)
    focused = run("xdotool", "getwindowfocus", env=env).strip()
    if int(focused) != int(windows[0], 16):
        raise RuntimeError("Keyboard focus is not on the Samsung login window")
    run("xdotool", "type", "--clearmodifiers", secrets.token_hex(24), env=env)
    submitted = True
    run("xdotool", "key", "Return", env=env)
    run("xwd", "-root", "-silent", "-out", "/tmp/neon-dusk-auth-submission.xwd", env=env)
    Path("/tmp/neon-dusk-auth-submission.xwd").chmod(0o644)
    deadline = time.monotonic() + 15
    while time.monotonic() < deadline:
        log = run("journalctl", "-b", "_SYSTEMD_INVOCATION_ID=" + invocation, "--no-pager")
        if "Authentication failure" in log or "Authentication error" in log:
            break
        time.sleep(0.2)
    else:
        raise RuntimeError("No password rejection was reported")
    time.sleep(0.5)
    run("xwd", "-root", "-silent", "-out", "/tmp/neon-dusk-auth-failure.xwd", env=env)
    Path("/tmp/neon-dusk-auth-failure.xwd").chmod(0o644)
    print("Real SDDM/PAM rejected the test password; failure screenshot captured.")
    print("Samsung login window focused automatically:", windows[0])
finally:
    run("systemctl", "stop", "sddm.service")
    run("chvt", "1")
    if submitted:
        run("faillock", "--user", "prayuj", "--reset")
