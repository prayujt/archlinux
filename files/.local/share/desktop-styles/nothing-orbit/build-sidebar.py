from pathlib import Path
import json, re, runpy

home = Path.home()
source = home / '.local/state/desktop-styles/before-sidebar/config'
target = home / '.local/share/desktop-styles/nothing-orbit/config'

def write(name, text):
    p = target / name
    p.parent.mkdir(parents=True, exist_ok=True)
    p.write_text(text)

def once(text, old, new):
    assert text.count(old) == 1, old[:100]
    return text.replace(old, new, 1)

s = (source / 'quickshell/shell.qml').read_text()
fields = {'bgColor': 'panel', 'textColor': 'paper', 'subtext': 'quiet', 'accent': 'accent',
          'clockColor': 'paper', 'volumeColor': 'paper', 'mutedColor': 'accent',
          'gainColor': 'paper', 'surface': 'surface', 'inkColor': 'ink', 'outline': 'outline'}
for key, name in fields.items():
    s, count = re.subn(r'(readonly property color '+key+r': )[^\n]+', r'\g<1>desktopPalette.'+name, s)
    assert count == 1, key
s = once(s, '    // -- Nothing / Orbit palette --', '''    DesktopPalette { id: desktopPalette }
    readonly property var palette: desktopPalette
    readonly property color desktopBackground: desktopPalette.background
    readonly property bool desktopReady: orbitDesktop.ready
    readonly property int sidebarWidth: 70
    readonly property int notificationCount: notifModel.count
    readonly property bool rayNeedsAttention: notifToast.waiting.some(key => notifToast.notifications[key]?.app === "Ray")

    // Shared monochrome palette.''')
s = once(s, '    OrbitalDesktop { shellState: root }', '''    OrbitalDesktop { id: orbitDesktop; shellState: root }
    GlyphSidebar { id: sidebar; shellState: root }''')
s = once(s, '            implicitHeight: isDp ? modelData.height : root.barHeight', '            implicitHeight: modelData.height\n            visible: bar.isDp')
s = once(s, '            exclusiveZone: root.barHeight', '            exclusiveZone: -1')
s = once(s, '                Region { item: barContent }', '                Region { item: barContent.visible ? barContent : null }')
s = once(s, '                Region { item: pillBg }', '                Region { item: pillBg.visible ? pillBg : null }')
s = once(s, '                id: barContent\n', '                id: barContent\n                visible: false\n')
s = once(s, '''                id: pillBg
                visible: bar.isDp
                anchors.horizontalCenter: parent.horizontalCenter''', '''                id: pillBg
                visible: bar.isDp && opacity > 0.001
                opacity: root.pillMode !== "clock" ? 1 : 0
                Behavior on opacity { NumberAnimation { duration: root.pillMode === "clock" ? 180 : 120; easing.type: root.pillMode === "clock" ? Easing.InCubic : Easing.OutCubic } }
                x: root.telegramFullscreen ? 0 : root.sidebarWidth + 6''')
s = once(s, '                y: root.telegramFullscreen ? 0 : root.barInset', '                y: root.telegramFullscreen ? 0 : 6')
s = once(s, 'x: (tgDismissOverlay.width - 430) / 2', 'x: root.sidebarWidth + 6')
s = once(s, 'y: root.barInset\n                width: 430', 'y: 6\n                width: 430')
s = once(s, '                height: 360\n            }\n        }\n\n        MouseArea', '''                height: 360
            }
            Region {
                intersection: Intersection.Subtract
                width: root.sidebarWidth
                height: tgDismissOverlay.height
            }
        }

        MouseArea''')
s = once(s, '''                id: utilityBar
                visible: bar.isDp && !root.newsPanelOpen''', '''                id: utilityBar
                visible: false''')
s = once(s, '''                anchors.horizontalCenter: parent.horizontalCenter
                // Hold the exit position''', '''                x: root.sidebarWidth + 6
                // Hold the exit position''')
s = once(s, 'return JSON.stringify({desktopStyle: "nothing-orbit", launcher:', 'return JSON.stringify({desktopStyle: "nothing-orbit", sidebar: true, desktopReady: root.desktopReady, appearance: desktopPalette.mode, sidebarMenu: sidebar.menu, launcher:')
s = once(s, 'opacity: (root.pillMode === "clock" && !pillBg.compactMedia) ? pillBg.morphCloseness : 0', 'opacity: 0')
for mode in ['launcher', 'telegram', 'notif']:
    line = 'opacity: root.pillMode === "' + mode + '" ? pillBg.morphCloseness : 0'
    animation = '\n                    Behavior on opacity { NumberAnimation { duration: root.pillMode === "' + mode + '" ? 80 : 180; easing.type: root.pillMode === "' + mode + '" ? Easing.OutCubic : Easing.InCubic } }'
    s = once(s, line, line.replace('pillBg.morphCloseness', '1') + animation)
pill_start = s.index('                id: pillBg\n')
pill_end = s.index('                // -- clock face --', pill_start)
s = s[:pill_start] + s[pill_start:pill_end].replace('duration: 280', 'duration: 180').replace('easing.type: Easing.BezierSpline; easing.bezierCurve: [0.16, 1, 0.3, 1, 1, 1]', 'easing.type: root.pillMode === "clock" ? Easing.InCubic : Easing.OutCubic') + s[pill_end:]
write('quickshell/shell.qml', s)

s = 'pragma ComponentBehavior: Bound\n' + (source / 'quickshell/OrbitalDesktop.qml').read_text()
for key, field in [('paper', 'textColor'), ('quiet', 'subtext'), ('red', 'accent')]:
    s = re.sub(r'(readonly property color '+key+r': )[^\n]+', r'\g<1>shellState.'+field, s)
s = once(s, '    readonly property string mono:', '''    readonly property color background: typeof shellState.desktopBackground !== "undefined" ? shellState.desktopBackground : "#08090b"
    readonly property color card: typeof shellState.palette !== "undefined" ? shellState.palette.card : "#101113"
    readonly property color border: shellState.outline
    readonly property bool light: background.r > 0.5
    readonly property string mono:''')
s = s.replace('color: "#08090b"', 'color: desktop.background')
s = s.replace('color: "#e6101113"; border.color: "#282b2d"', 'color: desktop.card; border.color: desktop.border')
s = s.replace('ctx.fillStyle = "#17191b";', 'ctx.fillStyle = desktop.light ? "#d4d5ce" : "#17191b";')
s = s.replace('ctx.fillStyle = Qt.rgba(0.88, 0.89, 0.86, Math.min(0.88, value + 0.10));', 'ctx.fillStyle = desktop.light ? Qt.rgba(0.12, 0.14, 0.12, Math.min(0.88, value + 0.10)) : Qt.rgba(0.88, 0.89, 0.86, Math.min(0.88, value + 0.10));')
s = s.replace('ctx.fillStyle = i % 9 === 0 ? "#747875" : "#34383a";', 'ctx.fillStyle = desktop.light ? "#93988e" : i % 9 === 0 ? "#747875" : "#34383a";')
s = once(s, '                    renderStrategy: Canvas.Threaded', '''                    renderStrategy: Canvas.Threaded
                    Connections { target: desktop; function onBackgroundChanged() { cosmos.requestPaint(); } }''')
s = s.replace('desktop.paper : \"#343638\"', 'desktop.paper : desktop.light ? \"#c8cdc3\" : \"#343638\"')
start = s.index('                Canvas {\n')
end = s.index('\n                Item {\n                    x: 105', start)
s = s[:start] + '''                Canvas {
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
                    x: parent.width * 0.735 - width / 2
                    y: parent.height * 0.51 - height / 2
                    width: parent.height * 0.63; height: width
                    now: desktop.now
                }
''' + s[end:]
s = once(s, 'memoryGiB: 0, uptime:', 'memoryGiB: 0, memoryTotalGiB: 0, uptime:')
s = once(s, '''                    Caption {
                        y: 397 * wall.s
                        text: "A LITTLE SPACE TO THINK."
                        font.pixelSize: 10 * wall.s
                    }
''', '')
s = once(s, 'value: desktop.metrics.memoryGiB + "G"', 'value: desktop.metrics.memoryTotalGiB ? desktop.metrics.memoryGiB.toFixed(1) + "G / " + desktop.metrics.memoryTotalGiB.toFixed(1) + "G" : "—"')
s = once(s, 'font.pixelSize: (modelData.label === "UPTIME" ? 24 : 36) * wall.s', '''font.pixelSize: (modelData.label === "CPU" ? 36 : 24) * wall.s
                                    width: parent.width - 44 * wall.s
                                    fontSizeMode: Text.Fit; minimumPixelSize: 18 * wall.s''')
s = once(s, 'font.letterSpacing: 2.1 * wall.s', '''font.letterSpacing: 2.1 * wall.s
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
                        }''')
s = once(s, 'item: mediaCard.visible ? mediaCard : null', '''regions: [
                    Region { item: mediaCard.visible ? mediaCard : null },
                    Region { x: 99 * wall.s; y: 364 * wall.s; width: dateCaption.width + 12 * wall.s; height: dateCaption.height + 12 * wall.s }
                ]''')
s = once(s, '''                    Caption {
                        y: 258''', '''                    Caption {
                        id: dateCaption
                        y: 258''')
s = once(s, '    readonly property var now: clock.date', '''    readonly property var now: clock.date
    readonly property bool ready: metrics.cpu !== null && metrics.memoryTotalGiB > 0
        && dotFace.status === FontLoader.Ready
        && wallpapers.instances.length === Quickshell.screens.length && wallpapers.instances.length > 0
        && wallpapers.instances.every(surface => surface.assetsReady)''')
s = once(s, '            color: desktop.background', '            color: "#000000"')
s = once(s, '            function capture(path: string): void {', '''            readonly property bool assetsReady: earth.ready
            function capture(path: string): void {''')
s = once(s, '                id: stage\n                anchors.fill: parent', '''                id: stage
                anchors.fill: parent
                opacity: desktop.ready ? 1 : 0
                Behavior on opacity { NumberAnimation { duration: 200; easing.type: Easing.OutCubic } }''')
s = once(s, '                EarthGlobe {', '                EarthGlobe {\n                    id: earth')
write('quickshell/OrbitalDesktop.qml', s)

s = (source / 'hypr/hyprland.lua').read_text()
s = once(s, 'leaf = "workspaces",       enabled = true, speed = 3, bezier = "md3_standard", style = "fade"', 'leaf = "workspaces",       enabled = true, speed = 4, bezier = "md3_decel", style = "slidevert"')
s = once(s, 'monitor = "DP-3", default = true, persistent = true', 'monitor = "DP-3", default = true, persistent = true, gaps_out = {top = 6, right = 0, bottom = 6, left = 0}')
s = once(s, 'workspace = tostring(i), monitor = "DP-3"', 'workspace = tostring(i), monitor = "DP-3", gaps_out = {top = 6, right = 0, bottom = 6, left = 0}')
s = once(s, '/home/prayuj/.local/bin/desktop-style original', '/home/prayuj/.local/bin/desktop-style meadow')
s += '\nhl.bind("SUPER + CTRL + ALT + M", hl.dsp.exec_cmd("/home/prayuj/.local/bin/desktop-style toggle"))\n'
s = once(s, 'disable_hyprland_logo = true,', 'disable_hyprland_logo = true,\n        background_color = "rgb(000000)",')
write('hypr/hyprland.lua', s)
for name in ['alacritty/alacritty.toml']:
    write(name, (source / name).read_text())
mode = json.loads((home / '.local/state/desktop-styles/appearance.json').read_text()).get('mode', 'dark')
runpy.run_path(str(target.parent / 'apply-palette.py'))['apply'](mode, live=False)
print('Sidebar profile built. Existing keybinding definitions were preserved.')

for name in ['GlyphSidebar.qml', 'DesktopPalette.qml', 'SidebarSlider.qml']:
    write('quickshell/' + name, (home / '.config/quickshell' / name).read_text())

black = target.parent / 'assets/startup-black.png'
write('hypr/hyprpaper.conf', '\n'.join('wallpaper {\n  monitor = ' + output + '\n  path = ' + str(black) + '\n  fit_mode = cover\n}\n' for output in ['DP-3', 'HDMI-A-1']) + '\nsplash = false\n')
