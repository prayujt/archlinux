pragma ComponentBehavior: Bound
import Quickshell
import Quickshell.Io
import Quickshell.Wayland
import QtQuick
import QtQuick.Layouts

Scope {
    id: desktop
    required property var shellState
    property var metrics: ({cpu: null, memory: 0, memoryGiB: 0, memoryTotalGiB: 0, uptime: "—"})
    readonly property color paper: shellState.textColor
    readonly property color quiet: shellState.subtext
    readonly property color red: shellState.accent
    readonly property color background: typeof shellState.desktopBackground !== "undefined" ? shellState.desktopBackground : "#08090b"
    readonly property color card: typeof shellState.palette !== "undefined" ? shellState.palette.card : "#101113"
    readonly property color border: shellState.outline
    readonly property bool light: background.r > 0.5
    readonly property string mono: "SpaceMono Nerd Font Mono"
    readonly property var now: clock.date
    readonly property bool ready: metrics.cpu !== null && metrics.memoryTotalGiB > 0
        && dotFace.status === FontLoader.Ready
        && wallpapers.instances.length === Quickshell.screens.length && wallpapers.instances.length > 0
        && wallpapers.instances.every(surface => surface.assetsReady)

    FontLoader {
        id: dotFace
        source: "file:///home/prayuj/.local/share/desktop-styles/nothing-orbit/assets/Doto.ttf"
    }
    SystemClock { id: clock; precision: SystemClock.Seconds }
    Process {
        running: true
        command: ["/usr/bin/python3", "-u", "/home/prayuj/.local/share/desktop-styles/nothing-orbit/metrics.py"]
        stdout: SplitParser {
            onRead: data => { try { desktop.metrics = JSON.parse(data) } catch (e) {} }
        }
    }
    IpcHandler {
        target: "orbit"
        function capture(output: string, path: string): void {
            for (const surface of wallpapers.instances)
                if (surface.screen.name === output)
                    surface.capture(path);
        }
    }

    component Caption: Text {
        color: desktop.quiet
        font.family: desktop.mono
        font.pixelSize: 11
        font.letterSpacing: 2
    }

    Variants {
        id: wallpapers
        model: Quickshell.screens
        PanelWindow {
            id: wall
            required property var modelData
            screen: modelData
            anchors { top: true; bottom: true; left: true; right: true }
            exclusiveZone: -1
            WlrLayershell.layer: WlrLayer.Bottom
            WlrLayershell.namespace: "nothing-orbit-desktop"
            WlrLayershell.keyboardFocus: WlrKeyboardFocus.None
            color: "#000000"
            readonly property real s: Math.min(width / 1920, height / 1080)
            readonly property bool assetsReady: earth.ready
            function capture(path: string): void {
                stage.grabToImage(result => result.saveToFile(path));
            }
            mask: Region {
                regions: [
                    Region { item: mediaCard.visible ? mediaCard : null },
                    Region { x: 99 * wall.s; y: 364 * wall.s; width: dateCaption.width + 12 * wall.s; height: dateCaption.height + 12 * wall.s }
                ]
            }

            Item {
                id: stage
                anchors.fill: parent
                opacity: desktop.ready ? 1 : 0
                Behavior on opacity { NumberAnimation { duration: 200; easing.type: Easing.OutCubic } }
                Rectangle { anchors.fill: parent; color: desktop.background }
                Canvas {
                    id: cosmos
                    anchors.fill: parent
                    renderStrategy: Canvas.Threaded
                    Connections { target: desktop; function onBackgroundChanged() { cosmos.requestPaint(); } }
                    onWidthChanged: requestPaint()
                    onHeightChanged: requestPaint()
                    onPaint: {
                        const ctx = getContext("2d"), w = width, h = height, s = wall.s;
                        ctx.clearRect(0, 0, w, h);
                        ctx.fillStyle = desktop.light ? "#d4d5ce" : "#17191b";
                        for (let y = 18 * s; y < h; y += 30 * s)
                            for (let x = 18 * s; x < w; x += 30 * s) {
                                ctx.beginPath(); ctx.arc(x, y, 0.75 * s, 0, Math.PI * 2); ctx.fill();
                            }
                        ctx.font = (10 * s) + "px monospace";
                        for (let i = 0; i < 110; i++) {
                            const x = ((i * 137.508) % 1000) / 1000 * w;
                            const y = ((i * 83.371 + 41) % 1000) / 1000 * h;
                            if (x < w * 0.46 || Math.hypot((x - w * 0.735) / (h * 0.30), (y - h * 0.51) / (h * 0.30)) < 1.15) continue;
                            ctx.fillStyle = desktop.light ? "#93988e" : i % 9 === 0 ? "#747875" : "#34383a";
                            ctx.fillText(i % 7 === 0 ? "+" : ".", x, y);
                        }
                    }
                }
                EarthGlobe {
                    id: earth
                    x: parent.width * 0.735 - width / 2
                    y: parent.height * 0.51 - height / 2
                    width: parent.height * 0.63; height: width
                    now: desktop.now
                }

                Item {
                    x: 105 * wall.s; y: 112 * wall.s
                    width: 650 * wall.s; height: 810 * wall.s
                    Text {
                        y: 54 * wall.s
                        text: Qt.formatDateTime(desktop.now, "HH:mm")
                        color: desktop.paper
                        font.family: dotFace.name
                        font.weight: Font.Bold
                        font.variableAxes: ({"ROND": 100})
                        font.pixelSize: 172 * wall.s
                        font.letterSpacing: -6 * wall.s
                    }
                    Caption {
                        id: dateCaption
                        y: 258 * wall.s
                        text: Qt.formatDateTime(desktop.now, "dddd · dd MMMM yyyy").toUpperCase()
                        font.pixelSize: 11 * wall.s
                        font.letterSpacing: 2.1 * wall.s
                        MouseArea {
                            anchors.fill: parent; anchors.margins: -6 * wall.s
                            property int taps: 0
                            property double lastTap: 0
                            onPressed: {
                                const time = Date.now();
                                taps = time - lastTap < 350 ? taps + 1 : 1;
                                lastTap = time;
                                if (taps === 3) {
                                    taps = 0;
                                    Quickshell.execDetached(["/home/prayuj/.local/bin/desktop-style", "toggle"]);
                                }
                            }
                        }
                    }
                    Text {
                        y: 342 * wall.s
                        text: {
                            const h = desktop.now.getHours();
                            return h < 12 ? "Good morning, Prayuj." : h < 18 ? "Good afternoon, Prayuj." : "Good evening, Prayuj.";
                        }
                        color: desktop.paper
                        font.family: "Adwaita Sans"
                        font.weight: Font.Light
                        font.pixelSize: 35 * wall.s
                    }

                    Row {
                        y: 482 * wall.s
                        spacing: 12 * wall.s
                        Repeater {
                            model: [
                                {label: "CPU", value: desktop.metrics.cpu === null ? "—" : desktop.metrics.cpu + "%", detail: "PROCESSOR", amount: (desktop.metrics.cpu || 0) / 100},
                                {label: "MEMORY", value: desktop.metrics.memoryTotalGiB ? desktop.metrics.memoryGiB.toFixed(1) + "G / " + desktop.metrics.memoryTotalGiB.toFixed(1) + "G" : "—", detail: desktop.metrics.memory + "% USED", amount: desktop.metrics.memory / 100},
                                {label: "UPTIME", value: desktop.metrics.uptime, detail: "SYSTEM UPTIME", amount: -1}
                            ]
                            Rectangle {
                                id: metricCard
                                required property var modelData
                                width: 196 * wall.s; height: 138 * wall.s
                                radius: 22 * wall.s; color: desktop.card; border.color: desktop.border; border.width: 1
                                Caption { x: 23 * wall.s; y: 20 * wall.s; text: modelData.label; font.pixelSize: 10 * wall.s; font.letterSpacing: 1.5 }
                                Text {
                                    x: 22 * wall.s; y: 43 * wall.s
                                    text: modelData.value; color: desktop.paper
                                    font.family: dotFace.name; font.weight: Font.Bold
                                    font.variableAxes: ({"ROND": 100})
                                    font.pixelSize: (modelData.label === "CPU" ? 36 : 24) * wall.s
                                    width: parent.width - 44 * wall.s
                                    fontSizeMode: Text.Fit; minimumPixelSize: 18 * wall.s
                                }
                                Row {
                                    x: 23 * wall.s; y: 99 * wall.s; spacing: 4 * wall.s
                                    visible: modelData.amount >= 0
                                    Repeater {
                                        model: 20
                                        Rectangle {
                                            required property int index
                                            width: 3.5 * wall.s; height: width; radius: width / 2
                                            color: index / 20 < metricCard.modelData.amount ? desktop.paper : desktop.light ? "#c8cdc3" : "#343638"
                                        }
                                    }
                                }
                                Caption {
                                    x: 23 * wall.s; y: 108 * wall.s
                                    visible: modelData.amount < 0
                                    text: modelData.detail; font.pixelSize: 8 * wall.s; font.letterSpacing: 1
                                }
                            }
                        }
                    }
                }

                Rectangle {
                    id: mediaCard
                    visible: desktop.shellState.sonosHasTrack
                    x: 105 * wall.s; y: 765 * wall.s
                    width: 612 * wall.s; height: 126 * wall.s
                    radius: 24 * wall.s; color: desktop.card; border.color: desktop.border; border.width: 1
                    Caption { x: 26 * wall.s; y: 22 * wall.s; text: "NOW PLAYING"; font.pixelSize: 9 * wall.s }
                    Text {
                        x: 26 * wall.s; y: 47 * wall.s; width: 450 * wall.s
                        text: desktop.shellState.sonosTitle
                        color: desktop.paper; font.family: "Adwaita Sans"; font.pixelSize: 23 * wall.s; elide: Text.ElideRight
                    }
                    Caption {
                        x: 26 * wall.s; y: 87 * wall.s; width: 450 * wall.s
                        text: desktop.shellState.sonosArtist
                        font.pixelSize: 9 * wall.s; font.letterSpacing: 1; elide: Text.ElideRight
                    }
                    Rectangle {
                        anchors.right: parent.right; anchors.rightMargin: 24 * wall.s; anchors.verticalCenter: parent.verticalCenter
                        width: 46 * wall.s; height: width; radius: width / 2; color: desktop.red
                        Text {
                            anchors.centerIn: parent; text: desktop.shellState.sonosPlaying ? "Ⅱ" : "▷"
                            color: desktop.paper; font.family: desktop.mono; font.pixelSize: 23 * wall.s
                        }
                    }
                    MouseArea {
                        anchors.fill: parent
                        cursorShape: desktop.shellState.sonosHasTrack ? Qt.PointingHandCursor : Qt.ArrowCursor
                        onClicked: if (desktop.shellState.sonosHasTrack) desktop.shellState.sonosTogglePlay()
                    }
                }

            }
        }
    }
}
