import Quickshell
import Quickshell.Io
import Quickshell.Wayland
import QtQuick
import QtQuick.Layouts

Scope {
    id: desktop
    required property var shellState
    property var metrics: ({cpu: null, memory: 0, memoryGiB: 0, uptime: "—"})
    readonly property color paper: "#e9e7e0"
    readonly property color quiet: "#8b8e8d"
    readonly property color red: "#e6393f"
    readonly property string mono: "SpaceMono Nerd Font Mono"
    readonly property var now: clock.date

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
            color: "#08090b"
            readonly property real s: Math.min(width / 1920, height / 1080)
            function capture(path: string): void {
                stage.grabToImage(result => result.saveToFile(path));
            }
            mask: Region {
                item: mediaCard.visible ? mediaCard : null
            }

            Item {
                id: stage
                anchors.fill: parent
                Rectangle { anchors.fill: parent; color: "#08090b" }
                Canvas {
                    id: cosmos
                    anchors.fill: parent
                    renderStrategy: Canvas.Threaded
                    onWidthChanged: requestPaint()
                    onHeightChanged: requestPaint()
                    onPaint: {
                        const ctx = getContext("2d");
                        const w = width, h = height, s = wall.s;
                        ctx.clearRect(0, 0, w, h);
                        // Stable, faint dot field. The ASCII scene only repaints on resize.
                        ctx.fillStyle = "#17191b";
                        for (let y = 18 * s; y < h; y += 30 * s)
                            for (let x = 18 * s; x < w; x += 30 * s) {
                                ctx.beginPath(); ctx.arc(x, y, 0.75 * s, 0, Math.PI * 2); ctx.fill();
                            }
                        const cx = w * 0.735, cy = h * 0.51, radius = h * 0.30;
                        const chars = " .,:;+=*#%@";
                        const cellX = 9 * s, cellY = 12 * s;
                        ctx.font = (10 * s) + "px monospace";
                        ctx.textAlign = "center"; ctx.textBaseline = "middle";
                        for (let y = cy - radius * 1.5; y < cy + radius * 1.5; y += cellY) {
                            for (let x = cx - radius * 2.05; x < cx + radius * 2.05; x += cellX) {
                                const nx = (x - cx) / radius, ny = (y - cy) / radius;
                                const rr = nx * nx + ny * ny;
                                const rx = nx * 0.966 + ny * 0.259;
                                const ry = -nx * 0.259 + ny * 0.966;
                                const ring = Math.sqrt(rx * rx / 2.85 + ry * ry / 0.15);
                                const ringHere = ring > 0.99 && ring < 1.22 && (rr > 1 || ry > 0);
                                let value = 0;
                                if (rr < 1) {
                                    const nz = Math.sqrt(1 - rr);
                                    const light = Math.max(0, -0.48 * nx - 0.42 * ny + 0.77 * nz);
                                    const clouds = 0.72 + 0.13 * Math.sin(ny * 27 + nz * 5 + Math.sin(nx * 12)) + 0.08 * Math.sin(ny * 63 + nx * 9);
                                    value = (0.06 + light * clouds) * Math.min(1, (1 - rr) * 19);
                                } else if (rr < 1.045) {
                                    value = (1.045 - rr) * 5;
                                }
                                if (ringHere) {
                                    const band = 0.35 + 0.38 * Math.abs(Math.sin(ring * 140));
                                    value = Math.max(value, band * (0.48 + 0.30 * (1 - Math.abs(rx) / 2)));
                                }
                                if (value > 0.045) {
                                    const index = Math.max(1, Math.min(chars.length - 1, Math.floor(value * (chars.length - 1))));
                                    ctx.fillStyle = Qt.rgba(0.88, 0.89, 0.86, Math.min(0.88, value + 0.10));
                                    ctx.fillText(chars[index], x, y);
                                }
                            }
                        }
                        // Sparse stars use deterministic positions; there is no animation loop.
                        for (let i = 0; i < 110; i++) {
                            const x = ((i * 137.508) % 1000) / 1000 * w;
                            const y = ((i * 83.371 + 41) % 1000) / 1000 * h;
                            if (x < w * 0.46 || Math.hypot((x - cx) / radius, (y - cy) / radius) < 1.35) continue;
                            ctx.fillStyle = i % 9 === 0 ? "#747875" : "#34383a";
                            ctx.fillText(i % 7 === 0 ? "+" : ".", x, y);
                        }
                    }
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
                        y: 258 * wall.s
                        text: Qt.formatDateTime(desktop.now, "dddd · dd MMMM yyyy").toUpperCase()
                        font.pixelSize: 11 * wall.s
                        font.letterSpacing: 2.1 * wall.s
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
                    Caption {
                        y: 397 * wall.s
                        text: "A LITTLE SPACE TO THINK."
                        font.pixelSize: 10 * wall.s
                    }

                    Row {
                        y: 482 * wall.s
                        spacing: 12 * wall.s
                        Repeater {
                            model: [
                                {label: "CPU", value: desktop.metrics.cpu === null ? "—" : desktop.metrics.cpu + "%", detail: "PROCESSOR", amount: (desktop.metrics.cpu || 0) / 100},
                                {label: "MEMORY", value: desktop.metrics.memoryGiB + "G", detail: desktop.metrics.memory + "% USED", amount: desktop.metrics.memory / 100},
                                {label: "UPTIME", value: desktop.metrics.uptime, detail: "SYSTEM UPTIME", amount: -1}
                            ]
                            Rectangle {
                                id: metricCard
                                required property var modelData
                                width: 196 * wall.s; height: 138 * wall.s
                                radius: 22 * wall.s; color: "#e6101113"; border.color: "#282b2d"; border.width: 1
                                Caption { x: 23 * wall.s; y: 20 * wall.s; text: modelData.label; font.pixelSize: 10 * wall.s; font.letterSpacing: 1.5 }
                                Text {
                                    x: 22 * wall.s; y: 43 * wall.s
                                    text: modelData.value; color: desktop.paper
                                    font.family: dotFace.name; font.weight: Font.Bold
                                    font.variableAxes: ({"ROND": 100})
                                    font.pixelSize: (modelData.label === "UPTIME" ? 24 : 36) * wall.s
                                }
                                Row {
                                    x: 23 * wall.s; y: 99 * wall.s; spacing: 4 * wall.s
                                    visible: modelData.amount >= 0
                                    Repeater {
                                        model: 20
                                        Rectangle {
                                            required property int index
                                            width: 3.5 * wall.s; height: width; radius: width / 2
                                            color: index / 20 < metricCard.modelData.amount ? desktop.paper : "#343638"
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
                    radius: 24 * wall.s; color: "#e6101113"; border.color: "#282b2d"; border.width: 1
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
