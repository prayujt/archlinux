pragma ComponentBehavior: Bound
import Quickshell
import Quickshell.Hyprland
import Quickshell.Io
import Quickshell.Wayland
import QtQuick

Scope {
    id: root
    property var snapshot: ({})
    property var preferences: ({city: ""})
    property string error: ""
    property var actions: []
    readonly property string directory: Quickshell.env("HOME") + "/.config/quickshell/dashboard/"
    readonly property var samsungMonitor: Hyprland.monitors.values.find(m => m.description.includes("HCJWA03310")) || null
    readonly property var samsungScreen: Quickshell.screens.find(s => s.name === root.samsungMonitor?.name) || null
    readonly property var homeWorkspace: Hyprland.workspaces.values.find(w => w.id === 1) || null
    readonly property var currentWorkspace: Hyprland.workspaces.values.find(w =>
        w.active && w.monitor?.name === root.samsungMonitor?.name) || null
    readonly property bool hasWindows: (root.currentWorkspace?.toplevels.values.length || 0) > 0

    function open() {
        if (root.homeWorkspace) root.homeWorkspace.activate();
        else if (Hyprland.usingLua) Hyprland.dispatch("hl.dsp.focus({ workspace = 1 })");
        else Hyprland.dispatch("workspace 1");
    }
    function save(arguments) {
        actions = actions.concat([arguments]);
        runAction();
    }
    function runAction() {
        if (!writer.running && actions.length) {
            writer.command = ["/usr/bin/python", directory + "data.py"].concat(actions[0]);
            actions = actions.slice(1);
            writer.running = true;
        }
    }
    FileView {
        id: preferencesFile
        path: Quickshell.env("HOME") + "/.local/state/quickshell-dashboard/preferences.json"
        watchChanges: true
        onFileChanged: reload()
        onLoaded: {
            try { root.preferences = JSON.parse(text()); }
            catch (error) { root.error = "Could not read your weather settings."; }
        }
    }
    Process {
        id: collector
        running: window.visible
        command: ["/usr/bin/python", root.directory + "data.py"]
        stdout: SplitParser {
            onRead: line => {
                try { root.snapshot = JSON.parse(line); }
                catch (error) { console.warn("Dashboard data:", error); }
            }
        }
        stderr: SplitParser { onRead: line => console.warn("Dashboard:", line) }
    }
    Process {
        id: writer
        stderr: StdioCollector { id: actionError }
        onExited: (code, status) => {
            root.error = code === 0 ? "" : actionError.text.trim();
            if (code === 0) preferencesFile.reload();
            Qt.callLater(root.runAction);
        }
    }
    IpcHandler {
        target: "dashboard"
        function open(): void { root.open(); }
        function status(): string {
            return JSON.stringify({visible: window.visible, hasWindows: root.hasWindows,
                clock: view.now.getTime(),
                monitor: root.samsungScreen?.name || "", workspace: root.currentWorkspace?.id || 0,
                stories: root.snapshot.news?.items?.length || 0, markets: root.snapshot.markets?.length || 0,
                forecast: !!root.snapshot.weather?.current, error: root.error,
                geometry: [window.width, window.height, view.width, view.height, view.opacity]});
        }
    }
    PanelWindow {
        id: window
        screen: root.samsungScreen
        visible: !!root.samsungScreen && !!root.currentWorkspace && !root.hasWindows
        onVisibleChanged: if (visible && view) view.now = new Date()
        anchors { top: true; bottom: true; left: true; right: true }
        margins.top: 40
        exclusionMode: ExclusionMode.Ignore
        color: "transparent"
        WlrLayershell.layer: WlrLayer.Bottom
        WlrLayershell.namespace: "welcome-dashboard"
        WlrLayershell.keyboardFocus: WlrKeyboardFocus.OnDemand
        Dashboard {
            id: view
            anchors.fill: parent
            snapshot: root.snapshot
            preferences: root.preferences
            error: root.error
            onSave: arguments => root.save(arguments)
        }
    }
}
