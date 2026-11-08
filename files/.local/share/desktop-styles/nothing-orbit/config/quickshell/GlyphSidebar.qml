pragma ComponentBehavior: Bound
import Quickshell
import Quickshell.Hyprland
import Quickshell.Io
import Quickshell.Wayland
import QtQuick

Scope {
    id: sidebar
    required property var shellState
    readonly property var palette: shellState.palette
    readonly property var ray: shellState.chatState("ray")
    property string menu: ""
    property string displayedMenu: ""
    property real menuOpacity: menu === "" ? 0 : 1
    Behavior on menuOpacity { NumberAnimation { duration: 180; easing.type: sidebar.menu === "" ? Easing.InCubic : Easing.OutCubic } }
    onMenuChanged: if (menu !== "") displayedMenu = menu
    property string output: ""
    property real popupY: 0
    property var mesh: ({connected: false, available: false, state: "Checking…", canControl: false, online: 0, total: 0, address: "", devices: [], error: ""})
    readonly property bool busy: meshCommand.running
    property var brightness: ({monitors: []})
    property var brightnessPending: ({})
    property string brightnessError: ""

    SystemClock { id: clock; precision: SystemClock.Minutes }
    FontLoader { id: dots; source: "file:///home/prayuj/.local/share/desktop-styles/nothing-orbit/assets/Doto.ttf" }
    Process {
        id: meshRead
        running: true
        command: ["/usr/bin/python3", "/home/prayuj/.local/share/desktop-styles/nothing-orbit/mesh-status.py"]
        stdout: SplitParser { onRead: data => { try { sidebar.mesh = JSON.parse(data) } catch (e) {} } }
    }
    Timer { interval: 10000; running: true; repeat: true; onTriggered: if (!sidebar.busy) meshRead.running = true }
    Process {
        id: meshCommand
        stdout: SplitParser { onRead: data => { try { sidebar.mesh = JSON.parse(data) } catch (e) {} } }
        onExited: meshRead.running = true
    }
    Process {
        id: brightnessRead
        running: true
        command: ["/home/prayuj/.config/hypr/setbrightness", "--status"]
        stdout: SplitParser {
            onRead: data => {
                const status = JSON.parse(data);
                if (JSON.stringify(status) !== JSON.stringify(sidebar.brightness)) sidebar.brightness = status;
            }
        }
        onExited: sidebar.sendBrightness()
    }
    Process {
        id: brightnessCommand
        onExited: (code, status) => {
            if (code !== 0) sidebar.brightnessError = "Couldn’t update monitor brightness.";
            if (Object.keys(sidebar.brightnessPending).length) sidebar.sendBrightness();
            else brightnessRead.running = true;
        }
    }
    function setBrightness(id: string, value: real): void {
        const amount = Math.max(0, Math.min(100, Math.round(value)));
        brightnessError = "";
        brightness = {monitors: brightness.monitors.map(m => m.id === id ? Object.assign({}, m, {value: amount}) : m)};
        brightnessPending[id] = amount;
        sendBrightness();
    }
    function sendBrightness(): void {
        if (brightnessRead.running || brightnessCommand.running) return;
        const id = Object.keys(brightnessPending)[0];
        if (!id) return;
        const amount = brightnessPending[id];
        delete brightnessPending[id];
        brightnessCommand.command = ["/home/prayuj/.config/hypr/setbrightness", id, String(amount)];
        brightnessCommand.running = true;
    }
    function setMesh(connected: bool): void {
        if (busy || !mesh.canControl) return;
        meshCommand.command = ["/usr/bin/python3", "/home/prayuj/.local/share/desktop-styles/nothing-orbit/mesh-status.py", connected ? "connect" : "disconnect"];
        meshCommand.running = true;
    }
    function show(name: string, screen: string, y: real): void {
        if (["appearance", "network", "mesh", "volume", "brightness"].indexOf(name) < 0) return;
        if (menu === name && output === screen) { menu = ""; return; }
        shellState.launcherOpen = false;
        shellState.telegramOpen = false;
        shellState.notifOpen = false;
        shellState.mediaExpanded = false;
        shellState.networkOpen = false;
        output = screen; popupY = y; menu = name;
        if (name === "brightness" && !brightnessRead.running && !brightnessCommand.running) brightnessRead.running = true;
    }
    Connections {
        target: sidebar.shellState
        function onAnyPanelChanged() { if (sidebar.shellState.anyPanel) sidebar.menu = ""; }
    }
    IpcHandler {
        target: "sidebar"
        function status(): string {
            return JSON.stringify({menu: sidebar.menu, appearance: sidebar.palette.mode,
                meshConnected: sidebar.mesh.connected, meshAvailable: sidebar.mesh.available,
                rayReady: sidebar.ray.ready, rayTyping: sidebar.ray.typing,
                brightness: sidebar.brightness.monitors,
                outputs: rails.instances.map(surface => ({name: surface.modelData.name, workspace: surface.activeId, dots: surface.dotCount}))});
        }
        function openMenu(name: string, output: string): void { sidebar.show(name, output, 650); }
        function closeMenu(): void { sidebar.menu = ""; }
        function appearance(mode: string): void { sidebar.palette.select(mode); }
        function workspace(output: string, number: int): void {
            for (const surface of rails.instances)
                if (surface.modelData.name === output && number >= 1 && number <= 10) surface.go(surface.first + number - 1);
        }
    }

    component Label: Text {
        color: sidebar.palette.paper
        font.family: "SpaceMono Nerd Font Mono"
        font.pixelSize: 11
    }
    component RailButton: Item {
        id: button
        property string glyph: ""
        property string kind: "icon"
        property bool selected: false
        property bool badge: false
        property bool online: true
        signal activated()
        signal secondary()
        signal scrolled(real delta)
        width: 46; height: 42
        Rectangle {
            anchors.centerIn: parent; width: 38; height: 36; radius: 13
            color: button.selected || buttonMouse.containsMouse ? sidebar.palette.surface : "transparent"
            Behavior on color { ColorAnimation { duration: 130 } }
        }
        Text {
            anchors.centerIn: parent
            visible: button.kind === "icon"
            text: button.glyph
            color: button.online ? sidebar.palette.paper : sidebar.palette.quiet
            font.family: "GeistMono Nerd Font Propo"
            font.pixelSize: 18
        }
        Canvas {
            id: symbol
            anchors.centerIn: parent; width: 24; height: 24
            visible: button.kind === "ray" || button.kind === "brightness"
            readonly property color ink: button.online ? sidebar.palette.paper : sidebar.palette.quiet
            onInkChanged: requestPaint()
            Connections { target: button; function onKindChanged() { symbol.requestPaint(); } }
            onPaint: {
                const ctx = getContext("2d");
                ctx.clearRect(0, 0, width, height);
                ctx.strokeStyle = ink; ctx.fillStyle = ink;
                ctx.lineWidth = 1.5; ctx.lineJoin = "round"; ctx.lineCap = "round";
                if (button.kind === "brightness") {
                    ctx.beginPath(); ctx.arc(12, 12, 4, 0, Math.PI * 2); ctx.stroke();
                    for (let i = 0; i < 8; i++) {
                        const angle = i * Math.PI / 4;
                        ctx.beginPath();
                        ctx.moveTo(12 + 7.5 * Math.cos(angle), 12 + 7.5 * Math.sin(angle));
                        ctx.lineTo(12 + 10 * Math.cos(angle), 12 + 10 * Math.sin(angle));
                        ctx.stroke();
                    }
                } else {
                    ctx.beginPath();
                    ctx.moveTo(12, 2);
                    ctx.bezierCurveTo(13.4, 9.2, 14.8, 10.6, 22, 12);
                    ctx.bezierCurveTo(14.8, 13.4, 13.4, 14.8, 12, 22);
                    ctx.bezierCurveTo(10.6, 14.8, 9.2, 13.4, 2, 12);
                    ctx.bezierCurveTo(9.2, 10.6, 10.6, 9.2, 12, 2);
                    ctx.fill();
                }

            }
        }
        Item {
            visible: button.kind === "mesh"
            width: 24; height: 24; anchors.centerIn: parent
            Repeater {
                model: 9
                Rectangle {
                    required property int index
                    x: (index % 3) * 8; y: Math.floor(index / 3) * 8
                    width: 4.5; height: 4.5; radius: width / 2
                    color: button.online ? sidebar.palette.paper : sidebar.palette.quiet
                    opacity: index === 0 || index === 4 || index === 8 ? 1 : 0.32
                }
            }
        }
        Rectangle {
            visible: button.badge; anchors.right: parent.right; anchors.top: parent.top
            anchors.rightMargin: 5; anchors.topMargin: 5
            width: 5; height: width; radius: width / 2; color: sidebar.palette.accent
        }
        MouseArea {
            id: buttonMouse
            anchors.fill: parent; hoverEnabled: true; cursorShape: Qt.PointingHandCursor
            acceptedButtons: Qt.LeftButton | Qt.RightButton
            onClicked: event => event.button === Qt.RightButton ? button.secondary() : button.activated()
            onWheel: event => { button.scrolled(event.angleDelta.y || event.pixelDelta.y); event.accepted = true; }
        }
    }
    component MenuButton: Rectangle {
        id: action
        property string label
        property bool accent: false
        signal activated()
        width: parent.width; height: 38; radius: 12
        color: actionMouse.containsMouse ? sidebar.palette.surface : sidebar.palette.card
        border.width: 1; border.color: accent ? sidebar.palette.accent : sidebar.palette.outline
        Label { anchors.centerIn: parent; text: action.label; color: action.accent ? sidebar.palette.accent : sidebar.palette.paper }
        MouseArea { id: actionMouse; anchors.fill: parent; hoverEnabled: true; cursorShape: Qt.PointingHandCursor; onClicked: action.activated() }
    }

    Variants {
        id: rails
        model: Quickshell.screens
        Scope {
            id: outputScope
            required property var modelData
            readonly property var monitor: Hyprland.monitorFor(modelData)
            readonly property int first: modelData.name === "DP-3" ? 1 : 11
            readonly property int activeId: monitor?.activeWorkspace?.id || first
            readonly property int dotCount: {
                let count = Math.max(6, activeId - first + 1);
                for (const ws of Hyprland.workspaces.values)
                    if (ws.id >= first && ws.id < first + 10 && ws.toplevels.values.length > 0) count = Math.max(count, ws.id - first + 1);
                return Math.min(10, count);
            }
            function go(id: int): void { Hyprland.dispatch("hl.dsp.focus({ workspace = " + id + " })"); }
            function popup(name: string, item: var): void { sidebar.show(name, modelData.name, item.mapToItem(null, 0, 0).y - 30); }

            PanelWindow {
                id: rail
                visible: outputScope.modelData.name === "DP-3"
                screen: outputScope.modelData
                anchors { top: true; bottom: true; left: true }
                implicitWidth: sidebar.shellState.sidebarWidth
                exclusiveZone: sidebar.shellState.sidebarWidth
                color: "transparent"
                WlrLayershell.layer: WlrLayer.Top
                WlrLayershell.namespace: "mono-sidebar"
                WlrLayershell.keyboardFocus: WlrKeyboardFocus.None
                mask: Region { item: pill }

                Rectangle {
                    id: pill
                    opacity: sidebar.shellState.desktopReady ? 1 : 0
                    Behavior on opacity { NumberAnimation { duration: 180; easing.type: Easing.OutCubic } }
                    x: 0; y: 4; width: 64; height: parent.height - 8; radius: 22
                    color: sidebar.palette.card; border.color: sidebar.palette.outline; border.width: 1
                    WheelHandler {
                        enabled: !controlsHover.hovered
                        target: null
                        onWheel: event => {
                            if (!event.angleDelta.y) return;
                            const step = event.angleDelta.y > 0 ? -1 : 1;
                            outputScope.go(outputScope.first + (outputScope.activeId - outputScope.first + step + 10) % 10);
                        }
                    }
                    Item {
                        id: launcher
                        x: 15; y: 20; width: 34; height: 34
                        Rectangle {
                            anchors.centerIn: parent
                            width: 7; height: 7; radius: 3.5
                            color: sidebar.palette.paper
                            opacity: launcherMouse.containsMouse || sidebar.shellState.launcherOpen ? 1 : 0.75
                            Behavior on opacity { NumberAnimation { duration: 120 } }
                        }
                        MouseArea {
                            id: launcherMouse; anchors.fill: parent; hoverEnabled: true; cursorShape: Qt.PointingHandCursor
                            onClicked: sidebar.shellState.launcherOpen = !sidebar.shellState.launcherOpen
                        }
                    }
                    Column {
                        id: workspaceDots
                        anchors.horizontalCenter: parent.horizontalCenter
                        y: 80
                        Repeater {
                            model: outputScope.dotCount
                            Item {
                                id: workspaceDot
                                required property int index
                                readonly property int ws: outputScope.first + index
                                readonly property bool current: outputScope.activeId === ws
                                readonly property bool occupied: (Hyprland.workspaces.values.find(w => w.id === ws)?.toplevels.values.length || 0) > 0
                                width: 40; height: current ? 36 : 20
                                Behavior on height { NumberAnimation { duration: 180; easing.type: Easing.OutCubic } }
                                Rectangle {
                                    anchors.centerIn: parent; width: 8; height: workspaceDot.current ? 28 : 8; radius: 4
                                    color: workspaceDot.current ? sidebar.palette.paper : workspaceDot.occupied ? sidebar.palette.quiet : sidebar.palette.muted
                                    Behavior on height { NumberAnimation { duration: 180; easing.type: Easing.OutCubic } }
                                    Behavior on color { ColorAnimation { duration: 130 } }
                                }
                                MouseArea { anchors.fill: parent; cursorShape: Qt.PointingHandCursor; onClicked: outputScope.go(workspaceDot.ws) }
                            }
                        }
                    }
                    Column {
                        id: timeStack
                        anchors.centerIn: parent
                        spacing: -4
                        Repeater {
                            model: [Qt.formatDateTime(clock.date, "HH"), Qt.formatDateTime(clock.date, "mm")]
                            Text {
                                required property string modelData
                                anchors.horizontalCenter: parent.horizontalCenter
                                text: modelData; color: sidebar.palette.paper
                                font.family: dots.name; font.pixelSize: 29; font.weight: Font.Bold; font.variableAxes: ({"ROND": 100})
                            }
                        }
                    }
                    Column {
                        id: controls
                        HoverHandler { id: controlsHover }
                        anchors.bottom: parent.bottom; anchors.bottomMargin: 18
                        anchors.horizontalCenter: parent.horizontalCenter
                        spacing: 3
                        RailButton {
                            kind: "ray"
                            selected: sidebar.shellState.telegramOpen && sidebar.shellState.telegramChat === "ray"
                            badge: sidebar.shellState.rayNeedsAttention
                            online: sidebar.ray.ready
                            onActivated: sidebar.shellState.toggleTelegram(false, "ray")
                            onSecondary: sidebar.shellState.toggleTelegram(true, "ray")
                        }
                        RailButton {
                            id: networkButton
                            glyph: "\uf1eb"
                            online: sidebar.shellState.networkStatus.connected
                            selected: sidebar.menu === "network" && sidebar.output === outputScope.modelData.name
                            onActivated: outputScope.popup("network", networkButton)
                        }
                        RailButton {
                            id: meshButton
                            kind: "mesh"
                            online: sidebar.mesh.connected; selected: sidebar.menu === "mesh" && sidebar.output === outputScope.modelData.name
                            onActivated: outputScope.popup("mesh", meshButton)
                        }
                        RailButton {
                            id: volumeButton
                            glyph: sidebar.shellState.muted ? "\uf026" : "\uf028"
                            selected: sidebar.menu === "volume" && sidebar.output === outputScope.modelData.name
                            onActivated: outputScope.popup("volume", volumeButton)
                            onSecondary: Quickshell.execDetached(["pactl", "set-sink-mute", "@DEFAULT_SINK@", "toggle"])
                            onScrolled: delta => { if (delta) Quickshell.execDetached(["pactl", "set-sink-volume", "@DEFAULT_SINK@", delta > 0 ? "+5%" : "-5%"]); }
                        }
                        RailButton {
                            id: brightnessButton
                            kind: "brightness"
                            selected: sidebar.menu === "brightness" && sidebar.output === outputScope.modelData.name
                            onActivated: outputScope.popup("brightness", brightnessButton)
                        }
                        RailButton {
                            glyph: "\uf0f3"
                            selected: sidebar.shellState.notifOpen; badge: sidebar.shellState.notificationCount > 0
                            onActivated: sidebar.shellState.notifOpen = !sidebar.shellState.notifOpen
                        }
                        RailButton {
                            visible: sidebar.shellState.sonosHasTrack
                            height: visible ? 42 : 0
                            glyph: "\uf001"
                            selected: sidebar.shellState.mediaExpanded
                            onActivated: sidebar.shellState.mediaExpanded = !sidebar.shellState.mediaExpanded
                        }
                    }
                }
            }

            PanelWindow {
                visible: outputScope.modelData.name === "HDMI-A-1"
                screen: outputScope.modelData
                anchors { top: true; bottom: true; left: true }
                implicitWidth: 14; exclusiveZone: -1; color: "transparent"
                WlrLayershell.layer: WlrLayer.Top
                WlrLayershell.namespace: "workspace-dots"
                WlrLayershell.keyboardFocus: WlrKeyboardFocus.None
                mask: Region { item: otherDots }
                Column {
                    id: otherDots
                    opacity: sidebar.shellState.desktopReady ? 1 : 0
                    anchors.centerIn: parent; width: 14
                    WheelHandler {
                        target: null
                        onWheel: event => {
                            if (event.angleDelta.y) outputScope.go(outputScope.first + (outputScope.activeId - outputScope.first + (event.angleDelta.y > 0 ? 9 : 1)) % 10);
                        }
                    }
                    Repeater {
                        model: outputScope.dotCount
                        Item {
                            id: otherDot
                            required property int index
                            readonly property int ws: outputScope.first + index
                            readonly property bool current: outputScope.activeId === ws
                            readonly property bool occupied: (Hyprland.workspaces.values.find(w => w.id === ws)?.toplevels.values.length || 0) > 0
                            width: 14; height: 22
                            Rectangle {
                                anchors.centerIn: parent; width: 4; height: otherDot.current ? 14 : 4; radius: 2
                                color: otherDot.current ? sidebar.palette.paper : otherDot.occupied ? sidebar.palette.quiet : sidebar.palette.muted
                                Behavior on height { NumberAnimation { duration: 150; easing.type: Easing.OutCubic } }
                            }
                            MouseArea { anchors.fill: parent; cursorShape: Qt.PointingHandCursor; onClicked: outputScope.go(otherDot.ws) }
                        }
                    }
                }
            }

            PanelWindow {
                visible: sidebar.menu !== "" && sidebar.output === outputScope.modelData.name
                screen: outputScope.modelData
                anchors { top: true; bottom: true; left: true; right: true }
                exclusiveZone: -1; color: "transparent"
                WlrLayershell.layer: WlrLayer.Overlay
                WlrLayershell.namespace: "mono-sidebar-dismiss"
                WlrLayershell.keyboardFocus: WlrKeyboardFocus.None
                MouseArea { anchors.fill: parent; onPressed: sidebar.menu = "" }
            }
            PanelWindow {
                id: popover
                visible: sidebar.menuOpacity > 0.001 && sidebar.output === outputScope.modelData.name
                screen: outputScope.modelData
                anchors { left: true; top: true }
                margins.left: sidebar.shellState.sidebarWidth + 6
                margins.top: Math.max(12, Math.min(sidebar.popupY, outputScope.modelData.height - implicitHeight - 12))
                implicitWidth: 310
                implicitHeight: menuContent.implicitHeight + 42
                exclusiveZone: -1; color: "transparent"
                WlrLayershell.layer: WlrLayer.Overlay
                WlrLayershell.namespace: "mono-sidebar-popover"
                WlrLayershell.keyboardFocus: sidebar.menu === "" ? WlrKeyboardFocus.None : WlrKeyboardFocus.Exclusive
                Rectangle {
                    anchors.fill: parent; radius: 24
                    opacity: sidebar.menuOpacity
                    scale: 0.97 + 0.03 * sidebar.menuOpacity
                    transformOrigin: Item.Left
                    color: sidebar.palette.card; border.color: sidebar.palette.outline; border.width: 1
                    focus: true
                    Keys.onEscapePressed: sidebar.menu = ""
                    Column {
                        id: menuContent
                        x: 21; y: 21; width: parent.width - 42
                        spacing: 16
                        Label { text: sidebar.displayedMenu.charAt(0).toUpperCase() + sidebar.displayedMenu.slice(1); font.pixelSize: 15 }
                        Column {
                            width: parent.width; spacing: 18; visible: sidebar.displayedMenu === "appearance"
                            Row {
                                spacing: 18; anchors.horizontalCenter: parent.horizontalCenter
                                Repeater {
                                    model: [{key: "dark", name: "Dark", color: "#30342f"}, {key: "light", name: "Light", color: "#eeeee7"}, {key: "black", name: "Black", color: "#050605"}]
                                    Item {
                                        id: choice
                                        required property var modelData
                                        width: 64; height: 82
                                        Rectangle {
                                            width: 49; height: 49; radius: 25; anchors.horizontalCenter: parent.horizontalCenter
                                            color: "transparent"; border.width: 1.5
                                            border.color: sidebar.palette.mode === choice.modelData.key ? sidebar.palette.accent : sidebar.palette.outline
                                            Rectangle { anchors.centerIn: parent; width: 38; height: 38; radius: 19; color: choice.modelData.color }
                                        }
                                        Label { anchors.horizontalCenter: parent.horizontalCenter; y: 60; text: choice.modelData.name; font.pixelSize: 10 }
                                        MouseArea { anchors.fill: parent; cursorShape: Qt.PointingHandCursor; onClicked: sidebar.palette.select(choice.modelData.key) }
                                    }
                                }
                            }
                            MenuButton {
                                label: "Previous top bar"
                                onActivated: Quickshell.execDetached(["/home/prayuj/.local/bin/desktop-style", "topbar"])
                            }
                        }
                        Column {
                            width: parent.width; spacing: 12; visible: sidebar.displayedMenu === "network"
                            Label { text: sidebar.shellState.networkStatus.connected ? "Connected" : "Disconnected"; font.pixelSize: 18 }
                            Label { text: sidebar.shellState.networkStatus.wireless ? "Wi-Fi" : "Ethernet"; color: sidebar.palette.quiet }
                            Label { text: sidebar.shellState.networkStatus.interface || "—"; color: sidebar.palette.quiet }
                            Label { text: sidebar.shellState.networkStatus.address || "—" }
                            Label { text: sidebar.shellState.networkStatus.speed || ""; visible: text.length > 0; color: sidebar.palette.quiet }
                        }
                        Column {
                            width: parent.width; spacing: 12; visible: sidebar.displayedMenu === "mesh"
                            Label { text: sidebar.mesh.state; font.pixelSize: 18 }
                            Label { text: sidebar.mesh.address || ""; visible: text.length > 0; color: sidebar.palette.quiet }
                            Label { text: sidebar.mesh.online + " / " + sidebar.mesh.total + " devices online"; color: sidebar.palette.quiet }
                            Column {
                                width: parent.width; spacing: 8
                                Repeater {
                                    model: sidebar.mesh.devices.slice(0, 5)
                                    Row {
                                        id: peerLine
                                        required property var modelData
                                        spacing: 10
                                        Rectangle { width: 5; height: 5; radius: 3; color: peerLine.modelData.online ? sidebar.palette.paper : sidebar.palette.muted; anchors.verticalCenter: parent.verticalCenter }
                                        Label { text: peerLine.modelData.name; width: 230; elide: Text.ElideRight; color: peerLine.modelData.online ? sidebar.palette.paper : sidebar.palette.quiet }
                                    }
                                }
                            }
                            Label { text: sidebar.mesh.error; visible: text.length > 0; width: parent.width; wrapMode: Text.Wrap; color: sidebar.palette.accent }
                            MenuButton {
                                enabled: sidebar.mesh.canControl && !sidebar.busy; opacity: enabled ? 1 : 0.4
                                label: sidebar.busy ? "Updating…" : sidebar.mesh.connected ? "Disconnect" : "Connect"
                                onActivated: sidebar.setMesh(!sidebar.mesh.connected)
                            }
                        }
                        Column {
                            width: parent.width; spacing: 16; visible: sidebar.displayedMenu === "brightness"
                            Repeater {
                                model: sidebar.brightness.monitors
                                Column {
                                    id: monitorBrightness
                                    required property var modelData
                                    width: parent.width; spacing: 6
                                    Label {
                                        width: parent.width
                                        text: monitorBrightness.modelData.name + "  " + (monitorBrightness.modelData.available ? Math.round(level.value) + "%" : "unavailable")
                                    }
                                    SidebarSlider {
                                        colors: sidebar.palette
                                        id: level
                                        width: parent.width; from: 0; to: 100; stepSize: 1
                                        enabled: monitorBrightness.modelData.available
                                        value: monitorBrightness.modelData.value || 0
                                        onPressedChanged: if (!pressed) sidebar.setBrightness(monitorBrightness.modelData.id, value)
                                        onMoved: if (!pressed) sidebar.setBrightness(monitorBrightness.modelData.id, value)
                                    }
                                }
                            }
                            Label { visible: brightnessRead.running; text: "Reading monitors…"; color: sidebar.palette.quiet }
                            Label { visible: sidebar.brightnessError.length > 0; text: sidebar.brightnessError; width: parent.width; wrapMode: Text.Wrap; color: sidebar.palette.accent }
                        }
                        Column {
                            width: parent.width; spacing: 16; visible: sidebar.displayedMenu === "volume"
                            Label { text: sidebar.shellState.muted ? "Muted" : sidebar.shellState.volume; font.pixelSize: 26 }
                            SidebarSlider {
                                colors: sidebar.palette
                                width: parent.width; from: 0; to: 100; stepSize: 1
                                value: parseInt(sidebar.shellState.volume) || 0
                                onMoved: Quickshell.execDetached(["pactl", "set-sink-volume", "@DEFAULT_SINK@", Math.round(value) + "%"])
                            }
                            MenuButton { label: sidebar.shellState.muted ? "Unmute" : "Mute"; onActivated: Quickshell.execDetached(["pactl", "set-sink-mute", "@DEFAULT_SINK@", "toggle"]) }
                        }
                    }
                }
            }
        }
    }
}
