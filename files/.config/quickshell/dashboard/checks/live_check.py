"""Check empty/occupied HP workspaces without moving or closing user windows."""
import json
from pathlib import Path
import subprocess
import time

DIRECTORY = Path(__file__).parents[1]
IPC = ["quickshell", "ipc", "-p", str(DIRECTORY / "shell.qml"), "call", "dashboard"]


def command(arguments):
    return subprocess.check_output(arguments, text=True)


def status():
    return json.loads(command(IPC + ["status"]))


def until(predicate, label):
    deadline = time.monotonic() + 5
    while time.monotonic() < deadline:
        if predicate():
            print("PASS:", label, flush=True)
            return
        time.sleep(0.1)
    raise AssertionError(label)


original = json.loads(command(["hyprctl", "activeworkspace", "-j"]))["id"]
original_monitors = json.loads(command(["hyprctl", "monitors", "-j"]))
original_cursor = json.loads(command(["hyprctl", "cursorpos", "-j"]))
clients = json.loads(command(["hyprctl", "clients", "-j"]))
occupied = {client["workspace"]["id"] for client in clients}
empty_workspaces = [workspace for workspace in range(12, 21) if workspace not in occupied]
assert len(empty_workspaces) >= 2, "This check requires two empty HP workspaces"
first, second = empty_workspaces[:2]
left_workspace = next(m["activeWorkspace"]["id"] for m in original_monitors if m["name"] == "DP-3")
apps = []
try:
    for workspace in [first, second]:
        command(["hyprctl", "dispatch", "hl.dsp.focus({ workspace = " + str(workspace) + " })"])
        until(lambda: status()["workspace"] == workspace and status()["visible"] and not status()["hasWindows"],
              "Dashboard visible on empty HP workspace " + str(workspace))
    until(lambda: status()["stories"] > 0 and status()["markets"] > 0, "Dashboard data loaded on HP")
    assert status()["monitor"] == "HDMI-A-1"
    layers = json.loads(command(["hyprctl", "layers", "-j"]))
    for monitor, details in layers.items():
        windows = [window for values in details["levels"].values() for window in values]
        own = [window for window in windows if window["namespace"] == "welcome-dashboard"]
        assert len(own) == (1 if monitor == "HDMI-A-1" else 0)
        if own:
            assert own[0] in details["levels"]["1"]
    print("PASS: Dashboard surface exists only on HP", flush=True)
    print("PASS: Dashboard uses the desktop layer below app windows", flush=True)
    command(["hyprctl", "dispatch", "hl.dsp.focus({ workspace = " + str(left_workspace) + " })"])
    assert status()["workspace"] == second and status()["visible"]
    print("PASS: Focusing the left monitor keeps the empty HP background visible", flush=True)
    command([str(DIRECTORY.parent / "scripts/dashboard")])
    until(lambda: status()["workspace"] == 11 and status()["visible"] == (11 not in occupied),
          "Home shortcut retains the same empty-workspace rule")
    command(["hyprctl", "dispatch", "hl.dsp.focus({ workspace = " + str(first) + " })"])
    until(lambda: status()["workspace"] == first and status()["visible"], "Background returns to the first test workspace")
    for index in range(2):
        app = subprocess.Popen(["alacritty", "--class", "dashboard-validation", "--title", "Dashboard validation",
                                "-e", "/usr/bin/sleep", "30"], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        apps.append(app)
        until(lambda: any(client["pid"] == app.pid and client["workspace"]["id"] == first
                          for client in json.loads(command(["hyprctl", "clients", "-j"]))),
              "Test window " + str(index + 1) + " opened on workspace " + str(first))
    until(lambda: status()["hasWindows"] and not status()["visible"], "Apps hide all widgets on their HP workspace")
    command(["hyprctl", "dispatch", "hl.dsp.focus({ workspace = " + str(second) + " })"])
    until(lambda: status()["workspace"] == second and status()["visible"] and not status()["hasWindows"],
          "Apps on another workspace do not hide the empty current workspace")
    command(["hyprctl", "dispatch", "hl.dsp.focus({ workspace = " + str(first) + " })"])
    until(lambda: status()["workspace"] == first and status()["hasWindows"] and not status()["visible"],
          "Returning to the occupied workspace hides widgets")
    apps[1].terminate()
    apps[1].wait(timeout=5)
    until(lambda: all(client["pid"] != apps[1].pid
                      for client in json.loads(command(["hyprctl", "clients", "-j"]))), "Second test window closed")
    assert status()["hasWindows"] and not status()["visible"]
    print("PASS: Widgets remain hidden while one app is still open", flush=True)
    apps[0].terminate()
    apps[0].wait(timeout=5)
    until(lambda: not status()["hasWindows"] and status()["visible"], "Closing the last window restores the background")
    assert abs(time.time() * 1000 - status()["clock"]) < 2000
    print("PASS: Clock updates immediately when the background returns", flush=True)
    command(["grim", "-o", "HDMI-A-1", str(DIRECTORY / "checks/dashboard-preview.png")])
finally:
    for app in apps:
        if app.poll() is None:
            app.terminate()
            app.wait(timeout=5)
    for monitor in original_monitors:
        command(["hyprctl", "dispatch", "hl.dsp.focus({ workspace = " + str(monitor["activeWorkspace"]["id"]) + " })"])
    command(["hyprctl", "dispatch", "hl.dsp.cursor.move({ x = " + str(original_cursor["x"])
             + ", y = " + str(original_cursor["y"]) + " })"])
    command(["hyprctl", "dispatch", "hl.dsp.focus({ workspace = " + str(original) + " })"])

print("Live monitor, workspace, shortcut, and automatic background checks passed.")
