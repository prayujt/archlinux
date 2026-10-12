pragma ComponentBehavior: Bound
import Quickshell
import Quickshell.Hyprland
import Quickshell.Io
import Quickshell.Wayland
import QtQuick

Scope {
    id: root
    property bool opened: false
    property int startingWorkspace: 1
    property int pendingWorkspace: 0
    property var pendingWindow: null
    property bool launchPending: Quickshell.env("WORKSPACE_WORLD_START_HIDDEN") !== "1"

    function show() {
        launchPending = false;
        const monitor = Hyprland.focusedMonitor;
        const screen = Quickshell.screens.find(s => s.name === monitor?.name);
        if (screen) window.screen = screen;
        startingWorkspace = Math.max(1, Math.min(20, monitor?.activeWorkspace?.id
                                               || monitor?.lastIpcObject?.activeWorkspace?.id
                                               || Hyprland.focusedWorkspace?.id || 1));
        opened = true;
    }
    function toggle() {
        if (opened) opened = false;
        else show();
    }
    function enterWorkspace(id, toplevel) {
        pendingWorkspace = id;
        pendingWindow = toplevel || null;
        opened = false;
        activateTimer.restart();
    }

    // Unmap the overlay before returning keyboard focus to the application.
    Timer {
        id: activateTimer
        interval: 80
        onTriggered: {
            const workspace = Hyprland.workspaces.values.find(w => w.id === root.pendingWorkspace);
            if (workspace) workspace.activate();
            else if (Hyprland.usingLua)
                Hyprland.dispatch("hl.dsp.focus({ workspace = " + root.pendingWorkspace + " })");
            else Hyprland.dispatch("workspace " + root.pendingWorkspace);
            if (root.pendingWindow?.wayland) root.pendingWindow.wayland.activate();
            root.pendingWindow = null;
        }
    }

    IpcHandler {
        target: "world"
        function toggle(): void { root.toggle(); }
        function open(): void { root.show(); }
        function hide(): void { root.opened = false; }
        function visit(workspace: int): void {
            if (workspace < 1 || workspace > 20) return;
            if (!root.opened) root.show();
            const world = scene.item as World;
            if (world) world.visit(workspace);
        }
        function status(): string {
            const world = scene.item as World;
            return JSON.stringify({ open: root.opened, scene: world ? world.status() : null });
        }
    }

    PanelWindow {
        id: window
        visible: root.opened
        anchors { top: true; bottom: true; left: true; right: true }
        exclusionMode: ExclusionMode.Ignore
        color: "#060d18"
        WlrLayershell.layer: WlrLayer.Overlay
        WlrLayershell.namespace: "workspace-world"
        WlrLayershell.keyboardFocus: root.opened ? WlrKeyboardFocus.Exclusive : WlrKeyboardFocus.None

        Loader {
            id: scene
            anchors.fill: parent
            focus: true
            active: root.opened
            sourceComponent: World {
                initialWorkspace: root.startingWorkspace
                onCloseRequested: root.opened = false
                onWorkspaceSelected: (id, toplevel) => root.enterWorkspace(id, toplevel)
            }
        }
    }

    Connections {
        target: Hyprland
        function onFocusedMonitorChanged() {
            if (root.launchPending && Hyprland.focusedMonitor) root.show();
        }
    }
    Component.onCompleted: if (launchPending && Hyprland.focusedMonitor) show()
}
