import Quickshell
import Quickshell.Hyprland
import Quickshell.Io
import Quickshell.Wayland
import QtQuick
import QtQuick.Layouts

Scope {
    id: root

    readonly property string fontFamily: "GeistMono Nerd Font Mono"

    // -- colors (tokyo night) --
    readonly property color bgColor: "#1a1b26"     // deep navy (pill fill)
    readonly property color textColor: "#c0caf5"   // pale blue-white
    readonly property color subtext: "#7a82ab"     // muted indigo
    readonly property color accent: "#7aa2f7"      // blue (focus, titles, selection)
    readonly property color clockColor: "#7dcfff"  // cyan (clock, media, sonos)
    readonly property color volumeColor: "#ff9e64" // warm orange (device volume)
    readonly property color mutedColor: "#f7768e"  // red-pink (losses)
    readonly property color gainColor: "#9ece6a"   // green (gains)
    readonly property color surface: "#292e42"     // raised navy (tracks, hover, placeholders)

    // -- shared state --
    property string time: ""
    property string volume: ""
    property bool muted: false
    property string activeWindow: ""
    property bool newsPanelOpen: true
    property bool launcherOpen: false
    property string launcherSearch: ""
    property int selectedIndex: 0
    property bool volumeOsd: false

    // -- sonos media state --
    property string sonosTitle: ""
    property string sonosArtist: ""
    property string sonosAlbum: ""
    property string sonosArt: ""
    property int sonosPos: 0
    property int sonosDur: 0
    property int sonosVolume: 0
    property bool sonosMuted: false
    property bool sonosVolOsd: false
    property string sonosState: "STOPPED"
    property bool sonosPlaying: sonosState === "PLAYING"
    property bool sonosHasTrack: (sonosState === "PLAYING" || sonosState === "PAUSED_PLAYBACK") && sonosTitle.length > 0
    property bool mediaExpanded: false

    // -- schwab portfolio state --
    property real portfolioValue: 0
    property real portfolioChange: 0
    property real portfolioChangePct: 0
    property real portfolioTotalPL: 0
    property real portfolioCash: 0
    property bool portfolioSample: true
    property bool marketOpen: false
    property var portfolioPositions: []
    property var portfolioSpark: []
    property bool portfolioExpanded: false

    property string pillMode: launcherOpen ? "launcher"
        : (mediaExpanded && sonosHasTrack) ? "media"
        : sonosVolOsd ? "sonosvol"
        : volumeOsd ? "volume"
        : "clock"

    // collapse media card if track stops
    onSonosHasTrackChanged: if (!sonosHasTrack) mediaExpanded = false

    // workspace state
    property string activeWsStr: ""
    property int focusedWs: -1

    function wsExists(id) {
        return activeWsStr.split(",").indexOf(id.toString()) !== -1
    }

    function fmtTime(secs) {
        if (secs < 0) secs = 0
        let m = Math.floor(secs / 60)
        let s = secs % 60
        return m + ":" + (s < 10 ? "0" + s : s)
    }

    function fmtMoney(n) {
        return "$" + Math.round(Math.abs(n)).toLocaleString(Qt.locale("en_US"), "f", 0)
    }

    // -- clock --
    SystemClock {
        id: sysClock
        precision: SystemClock.Seconds
        onDateChanged: root.time = Qt.formatDateTime(date, "ddd, dd MMM · h:mm:ss AP")
    }

    // -- volume: initial fetch --
    Process {
        id: volInitProc
        running: true
        command: ["sh", "-c", "pactl get-sink-volume @DEFAULT_SINK@ | grep -oP '\\d+%' | head -1 && pactl get-sink-mute @DEFAULT_SINK@"]
        stdout: SplitParser {
            onRead: data => {
                if (data.includes("%")) root.volume = data.trim()
                else if (data.includes("Mute:")) root.muted = data.includes("yes")
            }
        }
    }

    // -- volume: reactive via pactl subscribe --
    Process {
        id: volSubscribe
        running: true
        command: ["pactl", "subscribe"]
        stdout: SplitParser {
            onRead: data => {
                if (data.includes("sink") || data.includes("server"))
                    volRefresh.running = true
            }
        }
    }

    Process {
        id: volRefresh
        command: ["sh", "-c", "pactl get-sink-volume @DEFAULT_SINK@ | grep -oP '\\d+%' | head -1 && pactl get-sink-mute @DEFAULT_SINK@"]
        stdout: SplitParser {
            onRead: data => {
                if (data.includes("%")) {
                    let oldVol = root.volume
                    root.volume = data.trim()
                    if (oldVol !== "" && oldVol !== root.volume && !root.launcherOpen) {
                        root.volumeOsd = true
                        volOsdTimer.restart()
                    }
                } else if (data.includes("Mute:")) {
                    let wasMuted = root.muted
                    root.muted = data.includes("yes")
                    if (wasMuted !== root.muted && !root.launcherOpen) {
                        root.volumeOsd = true
                        volOsdTimer.restart()
                    }
                }
            }
        }
    }

    Timer {
        id: volOsdTimer
        interval: 700
        onTriggered: root.volumeOsd = false
    }

    // -- active window: initial fetch --
    Process {
        id: winInitProc
        running: true
        command: ["hyprctl", "activewindow", "-j"]
        stdout: SplitParser {
            splitMarker: ""
            onRead: data => {
                try {
                    let obj = JSON.parse(data)
                    let t = obj.title || ""
                    if (t.length > 40) t = t.substring(0, 37) + "..."
                    root.activeWindow = t
                } catch(e) { root.activeWindow = "" }
            }
        }
    }

    // -- hyprland event socket (shared for window + workspaces) --
    Process {
        id: hyprSubscribe
        running: true
        command: ["sh", "-c", "ncat -U $XDG_RUNTIME_DIR/hypr/$HYPRLAND_INSTANCE_SIGNATURE/.socket2.sock"]
        stdout: SplitParser {
            onRead: data => {
                if (data.includes("activewindow>>") || data.includes("activewindowv2>>") || data.includes("closewindow>>"))
                    winRefresh.running = true
                if (data.includes("workspace>>") || data.includes("createworkspace>>") || data.includes("destroyworkspace>>") || data.includes("movewindow>>") || data.includes("focusedmon>>")) {
                    wsRefresh.running = true
                    focusRefresh.running = true
                }
            }
        }
    }

    Process {
        id: winRefresh
        command: ["hyprctl", "activewindow", "-j"]
        stdout: SplitParser {
            splitMarker: ""
            onRead: data => {
                try {
                    let obj = JSON.parse(data)
                    let t = obj.title || ""
                    if (t.length > 40) t = t.substring(0, 37) + "..."
                    root.activeWindow = t
                } catch(e) { root.activeWindow = "" }
            }
        }
    }

    // -- workspaces: initial fetch --
    Process {
        id: wsInitProc
        running: true
        command: ["hyprctl", "workspaces", "-j"]
        stdout: SplitParser {
            splitMarker: ""
            onRead: data => {
                try {
                    let workspaces = JSON.parse(data)
                    let ids = []
                    for (let i = 0; i < workspaces.length; i++) ids.push(workspaces[i].id)
                    ids.sort((a, b) => a - b)
                    root.activeWsStr = ids.join(",")
                } catch(e) {}
            }
        }
    }

    Process {
        id: focusInitProc
        running: true
        command: ["hyprctl", "activeworkspace", "-j"]
        stdout: SplitParser {
            splitMarker: ""
            onRead: data => {
                try { root.focusedWs = JSON.parse(data).id } catch(e) {}
            }
        }
    }

    Process {
        id: wsRefresh
        command: ["hyprctl", "workspaces", "-j"]
        stdout: SplitParser {
            splitMarker: ""
            onRead: data => {
                try {
                    let workspaces = JSON.parse(data)
                    let ids = []
                    for (let i = 0; i < workspaces.length; i++) ids.push(workspaces[i].id)
                    ids.sort((a, b) => a - b)
                    root.activeWsStr = ids.join(",")
                } catch(e) {}
            }
        }
    }

    Process {
        id: focusRefresh
        command: ["hyprctl", "activeworkspace", "-j"]
        stdout: SplitParser {
            splitMarker: ""
            onRead: data => {
                try { root.focusedWs = JSON.parse(data).id } catch(e) {}
            }
        }
    }

    // -- news feed --
    property var headlines: []

    Process {
        id: newsProc
        running: true
        // -- News source: swap by commenting/uncommenting one `command:` below --

        // BBC US & Canada (US-focused hard news):
        // command: ["sh", "-c", "curl -s -A 'Mozilla/5.0' 'https://feeds.bbci.co.uk/news/world/us_and_canada/rss.xml' | python3 -c \"\nimport sys, xml.etree.ElementTree as ET, json\ntree = ET.parse(sys.stdin)\nns = {'media': 'http://search.yahoo.com/mrss/'}\nitems = []\nfor item in tree.findall('.//item')[:20]:\n    t = item.findtext('title', '')\n    d = item.findtext('description', '')\n    l = item.findtext('link', '').split('?')[0].replace('www.bbc.co.uk', 'www.bbc.com')\n    m = item.find('media:thumbnail', ns)\n    img = m.get('url', '') if m is not None else ''\n    items.append({'title': t, 'desc': d, 'image': img, 'link': l})\nprint(json.dumps(items))\n\""]

        // NYT HomePage (US-centric; needs the section block-list to drop opinion/features):
        // command: ["sh", "-c", "curl -s 'https://rss.nytimes.com/services/xml/rss/nyt/HomePage.xml' | python3 -c \"\nimport sys, xml.etree.ElementTree as ET, json\ntree = ET.parse(sys.stdin)\nns = {'media': 'http://search.yahoo.com/mrss/'}\nblock = ('/opinion/','/insider/','/cooking/','/style/','/t-magazine/','/arts/','/books/','/movies/','/theater/','/travel/','/realestate/','/magazine/','/well/','/food/','/dining/','/fashion/','/podcasts/','/crosswords/','/games/','/wirecutter/','/review/','/interactive/')\nitems = []\nfor item in tree.findall('.//item'):\n    l = item.findtext('link', '')\n    if any(b in l for b in block):\n        continue\n    t = item.findtext('title', '')\n    d = item.findtext('description', '')\n    m = item.find('media:content', ns)\n    img = m.get('url', '') if m is not None else ''\n    items.append({'title': t, 'desc': d, 'image': img, 'link': l})\n    if len(items) >= 20:\n        break\nprint(json.dumps(items))\n\""]

        // BBC World (active) — global hard news, heavy US coverage, minimal filtering:
        command: ["sh", "-c", "curl -s -A 'Mozilla/5.0' 'https://feeds.bbci.co.uk/news/world/rss.xml' | python3 -c \"\nimport sys, xml.etree.ElementTree as ET, json\ntree = ET.parse(sys.stdin)\nns = {'media': 'http://search.yahoo.com/mrss/'}\nitems = []\nfor item in tree.findall('.//item')[:20]:\n    t = item.findtext('title', '')\n    d = item.findtext('description', '')\n    l = item.findtext('link', '').split('?')[0].replace('www.bbc.co.uk', 'www.bbc.com')\n    m = item.find('media:thumbnail', ns)\n    img = m.get('url', '') if m is not None else ''\n    items.append({'title': t, 'desc': d, 'image': img, 'link': l})\nprint(json.dumps(items))\n\""]
        stdout: SplitParser {
            splitMarker: ""
            onRead: data => {
                try {
                    let items = JSON.parse(data.trim())
                    if (items.length > 0) {
                        root.headlines = items
                    }
                } catch(e) {}
            }
        }
    }

    // refresh headlines every 10 minutes
    Timer {
        interval: 600000; running: true; repeat: true
        onTriggered: newsProc.running = true
    }

    // -- news panel toggle listener (FIFO) --
    Process {
        id: newsToggleProc
        running: true
        command: ["sh", "-c", "rm -f /tmp/qs-news-toggle; mkfifo /tmp/qs-news-toggle; while true; do cat /tmp/qs-news-toggle; done"]
        stdout: SplitParser {
            onRead: data => {
                if (data.trim() === "toggle") root.newsPanelOpen = !root.newsPanelOpen
            }
        }
    }

    // -- launcher toggle listener (FIFO) --
    Process {
        id: launcherToggleProc
        running: true
        command: ["sh", "-c", "rm -f /tmp/qs-launcher-toggle; mkfifo /tmp/qs-launcher-toggle; while true; do cat /tmp/qs-launcher-toggle; done"]
        stdout: SplitParser {
            onRead: data => {
                if (data.trim() === "toggle") {
                    root.launcherOpen = !root.launcherOpen
                    if (!root.launcherOpen) root.launcherSearch = ""
                }
            }
        }
    }

    // -- portfolio card toggle listener (FIFO) --
    Process {
        id: portfolioToggleProc
        running: true
        command: ["sh", "-c", "rm -f /tmp/qs-portfolio-toggle; mkfifo /tmp/qs-portfolio-toggle; while true; do cat /tmp/qs-portfolio-toggle; done"]
        stdout: SplitParser {
            onRead: data => {
                if (data.trim() === "toggle") root.portfolioExpanded = !root.portfolioExpanded
            }
        }
    }

    // -- open URL --
    property string pendingUrl: ""
    Process {
        id: openUrl
        command: ["xdg-open", root.pendingUrl]
    }

    // -- volume up/down --
    Process {
        id: volUp
        command: ["pactl", "set-sink-volume", "@DEFAULT_SINK@", "+5%"]
    }

    Process {
        id: volDown
        command: ["pactl", "set-sink-volume", "@DEFAULT_SINK@", "-5%"]
    }

    // -- sonos: poll now-playing info --
    Process {
        id: sonosProc
        running: true
        command: ["/home/prayuj/.nix-profile/bin/python3", "/home/prayuj/.config/quickshell/scripts/sonos-info"]
        stdout: SplitParser {
            onRead: data => {
                try {
                    let m = JSON.parse(data.trim())
                    root.sonosTitle = m.title || ""
                    root.sonosArtist = m.artist || ""
                    root.sonosAlbum = m.album || ""
                    root.sonosArt = m.albumArt || ""
                    root.sonosDur = m.duration || 0
                    root.sonosPos = m.position || 0
                    root.sonosState = m.state || "STOPPED"
                    if (m.volume !== undefined && !root.sonosVolOsd)
                        root.sonosVolume = m.volume
                } catch(e) {}
            }
        }
    }

    // -- sonos volume OSD trigger (FIFO from sonos-vol script) --
    Process {
        id: sonosVolListen
        running: true
        command: ["sh", "-c", "rm -f /tmp/qs-sonos-vol; mkfifo /tmp/qs-sonos-vol; while true; do cat /tmp/qs-sonos-vol; done"]
        stdout: SplitParser {
            onRead: data => {
                let s = data.trim()
                if (s === "mute") {
                    root.sonosMuted = true
                } else {
                    let v = parseInt(s)
                    if (!isNaN(v)) {
                        root.sonosVolume = v
                        root.sonosMuted = false
                    }
                }
                root.sonosVolOsd = true
                sonosVolOsdTimer.restart()
            }
        }
    }

    Timer {
        id: sonosVolOsdTimer
        interval: 900
        onTriggered: root.sonosVolOsd = false
    }

    // interpolate position between polls
    Timer {
        interval: 1000; repeat: true
        running: root.sonosPlaying
        onTriggered: {
            if (root.sonosPos < root.sonosDur) root.sonosPos++
        }
    }

    // -- sonos controls --
    Process {
        id: sonosToggle
        command: ["/home/prayuj/.nix-profile/bin/python3", "/home/prayuj/.config/waybar/scripts/music/sonos-ctl", "toggle"]
    }
    Process {
        id: sonosNext
        command: ["/home/prayuj/.nix-profile/bin/python3", "/home/prayuj/.config/waybar/scripts/music/sonos-ctl", "next"]
    }
    Process {
        id: sonosPrev
        command: ["/home/prayuj/.nix-profile/bin/python3", "/home/prayuj/.config/waybar/scripts/music/sonos-ctl", "prev"]
    }

    // -- schwab portfolio: read-only poll (sample data until authorized) --
    Process {
        id: portfolioProc
        running: true
        command: ["/home/prayuj/.nix-profile/bin/python3", "/home/prayuj/.config/quickshell/scripts/schwab-portfolio"]
        stdout: SplitParser {
            onRead: data => {
                try {
                    let p = JSON.parse(data.trim())
                    root.portfolioValue = p.value || 0
                    root.portfolioChange = p.change || 0
                    root.portfolioChangePct = p.changePct || 0
                    root.portfolioTotalPL = p.totalPL || 0
                    root.portfolioCash = p.cash || 0
                    root.portfolioSample = p.sample === true
                    root.marketOpen = p.marketOpen === true
                    root.portfolioPositions = p.positions || []
                    root.portfolioSpark = p.spark || []
                } catch(e) {}
            }
        }
    }

    // refresh portfolio every 10s
    Timer {
        interval: 10000; running: true; repeat: true
        onTriggered: portfolioProc.running = true
    }

    // -- top bar per monitor --
    Variants {
        model: Quickshell.screens

        PanelWindow {
            id: bar
            required property var modelData
            screen: modelData

            property bool isDp: modelData.name === "DP-3"
            property int wsStart: isDp ? 1 : 11
            property int wsEnd: isDp ? 10 : 20

            anchors { top: true; left: true; right: true }
            implicitHeight: isDp ? 510 : 48
            color: "transparent"
            exclusiveZone: 48
            WlrLayershell.keyboardFocus: isDp && root.launcherOpen ? WlrKeyboardFocus.Exclusive : WlrKeyboardFocus.None

            mask: Region {
                Region { item: barContent }
                Region { item: pillBg }
                Region { item: portfolioCard }
            }

            Item {
                id: barContent
                anchors.left: parent.left
                anchors.right: parent.right
                anchors.top: parent.top
                height: 48

                Item {
                    anchors.fill: parent
                    anchors.leftMargin: 17
                    anchors.rightMargin: 17
                    anchors.topMargin: 4

                // == LEFT section ==
                Row {
                    id: leftSection
                    anchors.left: parent.left
                    anchors.verticalCenter: parent.verticalCenter
                    spacing: 8

                    // workspaces
                    Rectangle {
                        height: 40
                        width: wsRow.width + 20
                        color: root.bgColor
                        radius: 12
                        clip: true

                        Behavior on width {
                            NumberAnimation { duration: 150; easing.type: Easing.OutCubic }
                        }

                        Row {
                            id: wsRow
                            anchors.centerIn: parent
                            spacing: 4

                            Repeater {
                                model: 10

                                Item {
                                    required property int index
                                    property int wsId: bar.wsStart + index
                                    property int displayNum: index + 1
                                    property bool exists: root.wsExists(wsId)
                                    property bool isFocused: root.focusedWs === wsId
                                    property bool show: exists || isFocused

                                    width: show ? 30 : 0
                                    height: 30
                                    clip: true
                                    visible: width > 0

                                    Behavior on width {
                                        NumberAnimation { duration: 150; easing.type: Easing.OutCubic }
                                    }

                                    Rectangle {
                                        anchors.fill: parent
                                        radius: 8
                                        color: parent.isFocused ? root.surface : "transparent"

                                        Behavior on color {
                                            ColorAnimation { duration: 150 }
                                        }

                                        Text {
                                            anchors.centerIn: parent
                                            text: parent.parent.displayNum.toString()
                                            color: parent.parent.isFocused ? root.accent : root.subtext
                                            font.family: root.fontFamily
                                            font.pixelSize: 14
                                            font.bold: parent.parent.isFocused
                                        }
                                    }

                                    MouseArea {
                                        anchors.fill: parent
                                        onClicked: Hyprland.dispatch("workspace " + parent.wsId)
                                    }
                                }
                            }
                        }
                    }

                    // active window (DP-3 only)
                    Rectangle {
                        id: windowPill
                        height: 40
                        width: visible ? windowTextDisplay.implicitWidth + 24 : 0
                        color: root.bgColor
                        radius: 12
                        clip: true
                        visible: bar.isDp && root.activeWindow !== ""

                        Behavior on width {
                            NumberAnimation { duration: 200; easing.type: Easing.OutCubic }
                        }

                        property string displayedTitle: root.activeWindow
                        property string pendingTitle: ""

                        Connections {
                            target: root
                            function onActiveWindowChanged() {
                                if (root.activeWindow !== windowPill.displayedTitle) {
                                    windowPill.pendingTitle = root.activeWindow
                                    windowSlideAnim.start()
                                }
                            }
                        }

                        Text {
                            id: windowTextDisplay
                            anchors.centerIn: parent
                            text: windowPill.displayedTitle
                            color: root.accent
                            font.family: root.fontFamily
                            font.pixelSize: 13

                            property real slideY: 0
                            transform: Translate { y: windowTextDisplay.slideY }
                        }

                        SequentialAnimation {
                            id: windowSlideAnim

                            NumberAnimation {
                                target: windowTextDisplay; property: "slideY"
                                from: 0; to: -40
                                duration: 150; easing.type: Easing.InCubic
                            }

                            ScriptAction {
                                script: {
                                    windowPill.displayedTitle = windowPill.pendingTitle
                                    windowTextDisplay.slideY = 40
                                }
                            }

                            NumberAnimation {
                                target: windowTextDisplay; property: "slideY"
                                from: 40; to: 0
                                duration: 150; easing.type: Easing.OutCubic
                            }
                        }
                    }
                }

                // == news headline pill (DP-3) — fades out while the portfolio card is open ==
                Rectangle {
                    id: newsPill
                    visible: bar.isDp && root.newsPanelOpen && root.headlines.length > 0
                    anchors.right: parent.right
                    // hug the collapsed portfolio pill with an 8px gap (matches old spacing)
                    anchors.rightMargin: portfolioCard.collapsedW + 25
                    anchors.verticalCenter: parent.verticalCenter
                    height: 40
                    width: 380
                    color: root.bgColor
                    radius: 20
                    clip: true
                    opacity: root.portfolioExpanded ? 0 : 1
                    Behavior on opacity { NumberAnimation { duration: 200; easing.type: Easing.OutCubic } }

                    property int newsIndex: 0
                    // how far to scroll so the last char rests at the right edge (0 if it fits)
                    property real scrollTo: Math.min(0, newsClip.width - newsText.implicitWidth - 4)

                    onVisibleChanged: {
                        if (visible) newsCycle.restart()
                        else newsCycle.stop()
                    }

                    Connections {
                        target: root
                        function onHeadlinesChanged() {
                            newsPill.newsIndex = 0
                            if (newsPill.visible) newsCycle.restart()
                        }
                    }

                    // pulsing dot
                    Rectangle {
                        id: newsDot
                        anchors.left: parent.left
                        anchors.leftMargin: 17
                        anchors.verticalCenter: parent.verticalCenter
                        width: 7
                        height: 7
                        radius: 3.5
                        color: root.clockColor

                        SequentialAnimation on opacity {
                            running: newsPill.visible
                            loops: Animation.Infinite
                            NumberAnimation { to: 0.25; duration: 1100; easing.type: Easing.InOutSine }
                            NumberAnimation { to: 1.0; duration: 1100; easing.type: Easing.InOutSine }
                        }
                    }

                    // marquee viewport
                    Item {
                        id: newsClip
                        anchors.left: newsDot.right
                        anchors.leftMargin: 12
                        anchors.right: parent.right
                        anchors.rightMargin: 16
                        anchors.verticalCenter: parent.verticalCenter
                        height: parent.height
                        clip: true

                        Text {
                            id: newsText
                            anchors.verticalCenter: parent.verticalCenter
                            text: root.headlines.length > 0 ? (root.headlines[newsPill.newsIndex].title || "") : ""
                            color: root.textColor
                            font.family: root.fontFamily
                            font.pixelSize: 12

                            property real slideY: 0
                            transform: Translate { y: newsText.slideY }
                        }
                    }

                    // one headline at a time: reveal start, scroll to reveal end,
                    // pause, then slide up into the next headline
                    SequentialAnimation {
                        id: newsCycle
                        loops: Animation.Infinite

                        // show the beginning of the current headline
                        ScriptAction { script: { newsText.x = 0; newsText.slideY = 0 } }
                        PauseAnimation { duration: 1600 }

                        // scroll left just far enough to bring the last character into view
                        NumberAnimation {
                            target: newsText; property: "x"
                            to: newsPill.scrollTo
                            duration: Math.max(1, Math.abs(newsPill.scrollTo) * 22)
                            easing.type: Easing.InOutSine
                        }

                        // hold on the end for a beat
                        PauseAnimation { duration: 1100 }

                        // slide the current headline up and out
                        NumberAnimation {
                            target: newsText; property: "slideY"
                            to: -18; duration: 220; easing.type: Easing.InCubic
                        }

                        // advance to the next headline, reset below
                        ScriptAction {
                            script: {
                                if (root.headlines.length > 0)
                                    newsPill.newsIndex = (newsPill.newsIndex + 1) % root.headlines.length
                                newsText.x = 0
                                newsText.slideY = 18
                            }
                        }

                        // slide the next headline up into view
                        NumberAnimation {
                            target: newsText; property: "slideY"
                            to: 0; duration: 220; easing.type: Easing.OutCubic
                        }
                    }

                    MouseArea {
                        anchors.fill: parent
                        cursorShape: Qt.PointingHandCursor
                        onClicked: {
                            let h = root.headlines[newsPill.newsIndex]
                            if (h && h.link) { root.pendingUrl = h.link; openUrl.running = true }
                        }
                    }
                }

                }
            }

            // -- centered pill / launcher (DP-3 only) --
            Rectangle {
                id: pillBg
                visible: bar.isDp
                anchors.horizontalCenter: parent.horizontalCenter
                y: 6
                clip: true

                // compact now-playing shown in the collapsed pill while music plays
                property bool compactMedia: root.pillMode === "clock" && root.sonosHasTrack

                function modeW() {
                    if (root.pillMode === "launcher") return 400
                    if (root.pillMode === "media") return 390
                    if (root.pillMode === "volume") return 220
                    if (root.pillMode === "sonosvol") return root.sonosHasTrack ? 300 : 240
                    if (compactMedia) return 300
                    return 90
                }
                function modeH() {
                    if (root.pillMode === "launcher") return 500
                    if (root.pillMode === "media") return 150
                    if (root.pillMode === "sonosvol") return 68
                    return 40
                }

                width: modeW()
                height: modeH()
                color: root.bgColor
                radius: (root.pillMode === "launcher" || root.pillMode === "media") ? 16 : 20

                property real targetW: modeW()
                property real targetH: modeH()
                property real morphCloseness: {
                    let dw = Math.abs(width - targetW)
                    let dh = Math.abs(height - targetH)
                    let d = Math.max(dw, dh)
                    return 1 - Math.min(1, d / 400)
                }

                Behavior on width {
                    NumberAnimation { duration: 280; easing.bezierCurve: [0.16, 1, 0.3, 1, 1, 1] }
                }
                Behavior on height {
                    NumberAnimation { duration: 280; easing.bezierCurve: [0.16, 1, 0.3, 1, 1, 1] }
                }
                Behavior on radius {
                    NumberAnimation { duration: 280; easing.bezierCurve: [0.16, 1, 0.3, 1, 1, 1] }
                }

                // -- clock face --
                Text {
                    anchors.centerIn: parent
                    text: Qt.formatDateTime(new Date(), "HH:mm")
                    color: root.clockColor
                    font.family: root.fontFamily
                    font.pixelSize: 14
                    opacity: (root.pillMode === "clock" && !pillBg.compactMedia) ? pillBg.morphCloseness : 0
                    visible: opacity > 0.01

                    Behavior on opacity {
                        NumberAnimation { duration: 150 }
                    }

                    Timer {
                        interval: 5000; running: true; repeat: true
                        onTriggered: parent.text = Qt.formatDateTime(new Date(), "HH:mm")
                    }
                }

                // -- now-playing chip (compact + sonos-volume states share one header) --
                Item {
                    id: npChip
                    anchors.fill: parent
                    anchors.leftMargin: 8
                    anchors.rightMargin: 12
                    opacity: (root.pillMode === "sonosvol" || (root.pillMode === "clock" && root.sonosHasTrack)) ? 1 : 0
                    visible: opacity > 0.01

                    property bool volMode: root.pillMode === "sonosvol"

                    Behavior on opacity {
                        NumberAnimation { duration: 200; easing.type: Easing.OutCubic }
                    }

                    // album art — pinned to top so it never moves when the pill grows
                    Rectangle {
                        id: npArt
                        anchors.left: parent.left
                        anchors.top: parent.top
                        anchors.topMargin: 6
                        width: 28
                        height: 28
                        radius: 6
                        color: root.surface
                        clip: true
                        visible: root.sonosHasTrack

                        Text {
                            anchors.centerIn: parent
                            text: "\uf001"
                            font.family: "GeistMono Nerd Font Propo"
                            font.pixelSize: 12
                            color: root.subtext
                            visible: npArtImg.status !== Image.Ready
                        }

                        Image {
                            id: npArtImg
                            anchors.fill: parent
                            source: root.sonosArt
                            fillMode: Image.PreserveAspectCrop
                            asynchronous: true
                            cache: true
                            sourceSize.width: 56
                            sourceSize.height: 56
                        }
                    }

                    Text {
                        id: npTitle
                        anchors.left: root.sonosHasTrack ? npArt.right : parent.left
                        anchors.leftMargin: root.sonosHasTrack ? 10 : 0
                        anchors.right: npEq.left
                        anchors.rightMargin: 8
                        anchors.verticalCenter: npArt.verticalCenter
                        text: root.sonosHasTrack ? root.sonosTitle : "Sonos"
                        color: root.textColor
                        font.family: root.fontFamily
                        font.pixelSize: 13
                        elide: Text.ElideRight
                    }

                    // equalizer (only in compact, fades out in volume mode)
                    Row {
                        id: npEq
                        anchors.right: parent.right
                        anchors.verticalCenter: npArt.verticalCenter
                        spacing: 2
                        opacity: npChip.volMode ? 0 : 1

                        Behavior on opacity { NumberAnimation { duration: 150 } }

                        Repeater {
                            model: 3
                            Rectangle {
                                required property int index
                                width: 3
                                radius: 1.5
                                color: root.clockColor
                                anchors.verticalCenter: parent.verticalCenter
                                height: 6

                                SequentialAnimation on height {
                                    running: root.sonosPlaying && npChip.opacity > 0.5 && !npChip.volMode
                                    loops: Animation.Infinite
                                    NumberAnimation { to: 14; duration: 300 + index * 90; easing.type: Easing.InOutSine }
                                    NumberAnimation { to: 5; duration: 300 + index * 90; easing.type: Easing.InOutSine }
                                }
                            }
                        }
                    }

                    // volume bar — revealed underneath in volume mode, slides down
                    Item {
                        id: npVol
                        anchors.left: parent.left
                        anchors.right: parent.right
                        anchors.rightMargin: 2
                        anchors.bottom: parent.bottom
                        anchors.bottomMargin: 8
                        height: 12
                        opacity: npChip.volMode ? 1 : 0
                        visible: opacity > 0.01

                        Behavior on opacity {
                            NumberAnimation { duration: 180; easing.type: Easing.OutCubic }
                        }

                        transform: Translate {
                            y: npChip.volMode ? 0 : -10
                            Behavior on y {
                                NumberAnimation { duration: 260; easing.type: Easing.OutCubic }
                            }
                        }

                        Text {
                            id: npVolIcon
                            anchors.left: parent.left
                            anchors.verticalCenter: parent.verticalCenter
                            text: root.sonosMuted ? "\uf026" : root.sonosVolume === 0 ? "\uf026" : root.sonosVolume < 50 ? "\uf027" : "\uf028"
                            color: root.sonosMuted ? root.mutedColor : root.clockColor
                            font.family: "GeistMono Nerd Font Propo"
                            font.pixelSize: 12
                        }

                        Rectangle {
                            anchors.left: npVolIcon.right
                            anchors.leftMargin: 9
                            anchors.right: npVolNum.left
                            anchors.rightMargin: 8
                            anchors.verticalCenter: parent.verticalCenter
                            height: 4
                            radius: 2
                            color: root.surface

                            Rectangle {
                                width: root.sonosMuted ? 0 : Math.min(1, root.sonosVolume / 100) * parent.width
                                height: parent.height
                                radius: parent.radius
                                color: root.clockColor

                                Behavior on width {
                                    NumberAnimation { duration: 150; easing.type: Easing.OutCubic }
                                }
                            }
                        }

                        Text {
                            id: npVolNum
                            anchors.right: parent.right
                            anchors.verticalCenter: parent.verticalCenter
                            text: root.sonosMuted ? "\u00d7" : root.sonosVolume
                            color: root.sonosMuted ? root.mutedColor : root.clockColor
                            font.family: root.fontFamily
                            font.pixelSize: 11
                            width: 22
                            horizontalAlignment: Text.AlignRight
                        }
                    }

                    MouseArea {
                        anchors.fill: parent
                        cursorShape: Qt.PointingHandCursor
                        onClicked: root.mediaExpanded = true
                    }
                }

                // -- expanded media card face --
                Item {
                    id: mediaCard
                    anchors.fill: parent

                    // reveal tracks how far the card has actually expanded toward media size,
                    // so content fades in as the boundary grows and out as it shrinks.
                    // reaches full opacity at ~105px (well before the 150px target) so it
                    // doesn't lag behind the expansion
                    property real reveal: root.pillMode === "media"
                        ? Math.max(0, Math.min(1, (pillBg.height - 44) / (105 - 44)))
                        : 0
                    opacity: reveal
                    visible: opacity > 0.01

                    Behavior on opacity {
                        NumberAnimation { duration: 110; easing.type: Easing.OutCubic }
                    }

                    // collapse when clicking empty card area (buttons sit on top)
                    MouseArea {
                        anchors.fill: parent
                        onClicked: root.mediaExpanded = false
                    }

                    // album art (left, inset, rounded)
                    Rectangle {
                        id: mediaArt
                        anchors.left: parent.left
                        anchors.top: parent.top
                        anchors.bottom: parent.bottom
                        anchors.margins: 16
                        width: height
                        radius: 12
                        color: root.surface
                        clip: true

                        Text {
                            anchors.centerIn: parent
                            text: "\uf001"
                            font.family: "GeistMono Nerd Font Propo"
                            font.pixelSize: 34
                            color: root.subtext
                            visible: mediaArtImg.status !== Image.Ready
                        }

                        Image {
                            id: mediaArtImg
                            anchors.fill: parent
                            source: root.sonosArt
                            fillMode: Image.PreserveAspectCrop
                            asynchronous: true
                            cache: true
                            sourceSize.width: 240
                            sourceSize.height: 240
                        }

                        // subtle inner border for depth
                        Rectangle {
                            anchors.fill: parent
                            radius: parent.radius
                            color: "transparent"
                            border.width: 1
                            border.color: Qt.rgba(1, 1, 1, 0.06)
                        }
                    }

                    // content
                    Item {
                        anchors.left: mediaArt.right
                        anchors.leftMargin: 18
                        anchors.right: parent.right
                        anchors.rightMargin: 18
                        anchors.top: parent.top
                        anchors.bottom: parent.bottom

                        // title
                        Text {
                            id: mediaTitle
                            anchors.left: parent.left
                            anchors.right: parent.right
                            anchors.top: parent.top
                            anchors.topMargin: 22
                            text: root.sonosTitle
                            color: root.textColor
                            font.family: root.fontFamily
                            font.pixelSize: 15
                            font.bold: true
                            elide: Text.ElideRight
                        }

                        // artist
                        Text {
                            id: mediaArtist
                            anchors.left: parent.left
                            anchors.right: parent.right
                            anchors.top: mediaTitle.bottom
                            anchors.topMargin: 4
                            text: root.sonosArtist
                            color: root.subtext
                            font.family: root.fontFamily
                            font.pixelSize: 11
                            elide: Text.ElideRight
                            visible: text.length > 0
                        }

                        // transport controls (centered)
                        Row {
                            id: mediaTransport
                            anchors.horizontalCenter: parent.horizontalCenter
                            anchors.bottom: parent.bottom
                            anchors.bottomMargin: 42
                            spacing: 22
                            layoutDirection: Qt.LeftToRight

                            Text {
                                anchors.verticalCenter: parent.verticalCenter
                                text: "\uf048"
                                font.family: "GeistMono Nerd Font Propo"
                                font.pixelSize: 14
                                color: prevMouse.containsMouse ? root.textColor : root.subtext
                                Behavior on color { ColorAnimation { duration: 120 } }
                                MouseArea {
                                    id: prevMouse
                                    anchors.fill: parent
                                    anchors.margins: -10
                                    hoverEnabled: true
                                    cursorShape: Qt.PointingHandCursor
                                    onClicked: sonosPrev.running = true
                                }
                            }

                            // filled circular play/pause (spotify style)
                            Rectangle {
                                anchors.verticalCenter: parent.verticalCenter
                                width: 34
                                height: 34
                                radius: 17
                                color: playMouse.containsMouse ? Qt.lighter(root.clockColor, 1.12) : root.clockColor

                                Behavior on color { ColorAnimation { duration: 120 } }
                                Behavior on scale { NumberAnimation { duration: 120; easing.type: Easing.OutBack } }
                                scale: playMouse.pressed ? 0.92 : 1

                                Text {
                                    anchors.centerIn: parent
                                    anchors.horizontalCenterOffset: root.sonosPlaying ? 0 : 1
                                    text: root.sonosPlaying ? "\uf04c" : "\uf04b"
                                    font.family: "GeistMono Nerd Font Propo"
                                    font.pixelSize: 14
                                    color: root.bgColor
                                }

                                MouseArea {
                                    id: playMouse
                                    anchors.fill: parent
                                    hoverEnabled: true
                                    cursorShape: Qt.PointingHandCursor
                                    onClicked: sonosToggle.running = true
                                }
                            }

                            Text {
                                anchors.verticalCenter: parent.verticalCenter
                                text: "\uf051"
                                font.family: "GeistMono Nerd Font Propo"
                                font.pixelSize: 14
                                color: nextMouse.containsMouse ? root.textColor : root.subtext
                                Behavior on color { ColorAnimation { duration: 120 } }
                                MouseArea {
                                    id: nextMouse
                                    anchors.fill: parent
                                    anchors.margins: -10
                                    hoverEnabled: true
                                    cursorShape: Qt.PointingHandCursor
                                    onClicked: sonosNext.running = true
                                }
                            }
                        }

                        // progress bar + time
                        Item {
                            anchors.left: parent.left
                            anchors.right: parent.right
                            anchors.bottom: parent.bottom
                            anchors.bottomMargin: 16
                            height: 12

                            Text {
                                id: posLabel
                                anchors.left: parent.left
                                anchors.verticalCenter: mediaTrack.verticalCenter
                                text: root.fmtTime(root.sonosPos)
                                color: root.subtext
                                font.family: root.fontFamily
                                font.pixelSize: 9
                            }

                            Rectangle {
                                id: mediaTrack
                                anchors.left: posLabel.right
                                anchors.leftMargin: 8
                                anchors.right: durLabel.left
                                anchors.rightMargin: 8
                                anchors.verticalCenter: parent.verticalCenter
                                height: 3
                                radius: 1.5
                                color: root.surface

                                Rectangle {
                                    width: root.sonosDur > 0 ? Math.min(1, root.sonosPos / root.sonosDur) * parent.width : 0
                                    height: parent.height
                                    radius: parent.radius
                                    color: root.clockColor

                                    Behavior on width {
                                        NumberAnimation { duration: 900; easing.type: Easing.Linear }
                                    }
                                }
                            }

                            Text {
                                id: durLabel
                                anchors.right: parent.right
                                anchors.verticalCenter: mediaTrack.verticalCenter
                                text: root.fmtTime(root.sonosDur)
                                color: root.subtext
                                font.family: root.fontFamily
                                font.pixelSize: 9
                            }
                        }
                    }
                }

                // -- volume OSD face --
                Item {
                    anchors.fill: parent
                    anchors.margins: 10
                    opacity: root.pillMode === "volume" ? pillBg.morphCloseness : 0
                    visible: opacity > 0.01

                    Behavior on opacity {
                        NumberAnimation { duration: 150 }
                    }

                    // volume icon
                    Text {
                        id: volOsdIcon
                        anchors.left: parent.left
                        anchors.verticalCenter: parent.verticalCenter
                        text: root.muted ? "\uf026" : "\uf028"
                        color: root.muted ? root.mutedColor : root.volumeColor
                        font.family: "GeistMono Nerd Font Propo"
                        font.pixelSize: 14

                        Behavior on color { ColorAnimation { duration: 150 } }
                    }

                    // volume bar track
                    Rectangle {
                        id: volTrack
                        anchors.left: volOsdIcon.right
                        anchors.leftMargin: 8
                        anchors.right: volOsdText.left
                        anchors.rightMargin: 8
                        anchors.verticalCenter: parent.verticalCenter
                        height: 4
                        radius: 2
                        color: root.surface

                        // volume bar fill
                        Rectangle {
                            width: {
                                let pct = parseInt(root.volume) || 0
                                return Math.min(1, pct / 100) * parent.width
                            }
                            height: parent.height
                            radius: parent.radius
                            color: root.muted ? root.mutedColor : root.volumeColor

                            Behavior on width {
                                NumberAnimation { duration: 150; easing.type: Easing.OutCubic }
                            }
                            Behavior on color { ColorAnimation { duration: 150 } }
                        }
                    }

                    // volume percentage text
                    Text {
                        id: volOsdText
                        anchors.right: parent.right
                        anchors.verticalCenter: parent.verticalCenter
                        text: root.muted ? "muted" : root.volume
                        color: root.muted ? root.mutedColor : root.volumeColor
                        font.family: root.fontFamily
                        font.pixelSize: 12
                        width: 36
                        horizontalAlignment: Text.AlignRight

                        Behavior on color { ColorAnimation { duration: 150 } }
                    }
                }

                // -- launcher face --
                Column {
                    id: launcherContent
                    anchors.fill: parent
                    anchors.margins: 12
                    anchors.topMargin: 10
                    spacing: 8
                    opacity: root.pillMode === "launcher" ? pillBg.morphCloseness : 0
                    visible: opacity > 0.01

                    // filtered + sorted app list. Evaluated at startup (warms the
                    // desktop-entry index off the critical path) and on each search.
                    property var appList: {
                        let allApps = DesktopEntries.applications.values
                        let apps = []
                        let search = root.launcherSearch.toLowerCase()
                        for (let i = 0; i < allApps.length; i++) {
                            let app = allApps[i]
                            if (app.noDisplay) continue
                            if (search.length > 0) {
                                let name = (app.name || "").toLowerCase()
                                let generic = (app.genericName || "").toLowerCase()
                                let comment = (app.comment || "").toLowerCase()
                                let kw = app.keywords ? app.keywords.join(" ").toLowerCase() : ""
                                if (!name.includes(search) && !generic.includes(search) && !comment.includes(search) && !kw.includes(search))
                                    continue
                            }
                            apps.push(app)
                        }
                        apps.sort((a, b) => (a.name || "").localeCompare(b.name || ""))
                        return apps
                    }

                    Rectangle {
                        width: parent.width
                        height: 36
                        color: root.surface
                        radius: 10

                        TextInput {
                            id: searchInput
                            anchors.fill: parent
                            anchors.leftMargin: 12
                            anchors.rightMargin: 12
                            verticalAlignment: Text.AlignVCenter
                            color: root.textColor
                            font.family: root.fontFamily
                            font.pixelSize: 14
                            clip: true

                            onTextChanged: {
                                root.launcherSearch = text
                                root.selectedIndex = 0
                            }

                            Keys.onEscapePressed: {
                                root.launcherOpen = false
                                root.launcherSearch = ""
                                searchInput.text = ""
                            }

                            Keys.onReturnPressed: {
                                let idx = root.selectedIndex
                                let list = launcherContent.appList
                                if (idx >= 0 && idx < list.length) {
                                    let entry = list[idx]
                                    if (entry) {
                                        entry.execute()
                                        root.launcherOpen = false
                                        root.launcherSearch = ""
                                        searchInput.text = ""
                                        root.selectedIndex = 0
                                    }
                                }
                            }

                            Keys.onDownPressed: {
                                if (root.selectedIndex < launcherContent.appList.length - 1)
                                    root.selectedIndex++
                            }

                            Keys.onUpPressed: {
                                if (root.selectedIndex > 0)
                                    root.selectedIndex--
                            }

                            Keys.onPressed: event => {
                                if (event.modifiers & Qt.ControlModifier) {
                                    if (event.key === Qt.Key_J) {
                                        if (root.selectedIndex < launcherContent.appList.length - 1)
                                            root.selectedIndex++
                                        event.accepted = true
                                    } else if (event.key === Qt.Key_K) {
                                        if (root.selectedIndex > 0)
                                            root.selectedIndex--
                                        event.accepted = true
                                    }
                                }
                            }
                        }

                        Text {
                            anchors.left: parent.left
                            anchors.leftMargin: 12
                            anchors.verticalCenter: parent.verticalCenter
                            text: "Search apps..."
                            color: root.subtext
                            font.family: root.fontFamily
                            font.pixelSize: 14
                            visible: searchInput.text.length === 0
                        }
                    }

                    // virtualized list — only ~visible rows are instantiated,
                    // so opening is smooth even with hundreds of apps
                    ListView {
                        id: appListView
                        width: parent.width
                        height: parent.height - 44
                        clip: true
                        spacing: 2
                        model: launcherContent.appList
                        currentIndex: root.selectedIndex
                        boundsBehavior: Flickable.StopAtBounds
                        cacheBuffer: 400
                        reuseItems: true

                        delegate: Rectangle {
                            required property var modelData
                            required property int index
                            property var entry: modelData
                            property bool selected: index === root.selectedIndex
                            width: ListView.view.width
                            height: 42
                            color: selected ? root.accent : appMouse.containsMouse ? root.surface : "transparent"
                            radius: 8

                            Behavior on color {
                                ColorAnimation { duration: 100 }
                            }

                            Column {
                                anchors.left: parent.left
                                anchors.leftMargin: 12
                                anchors.verticalCenter: parent.verticalCenter
                                anchors.right: parent.right
                                anchors.rightMargin: 12

                                Text {
                                    text: modelData.name || ""
                                    color: parent.parent.selected ? root.bgColor : root.textColor
                                    font.family: root.fontFamily
                                    font.pixelSize: 13
                                    font.bold: parent.parent.selected
                                    elide: Text.ElideRight
                                    width: parent.width
                                }

                                Text {
                                    text: modelData.genericName || modelData.comment || ""
                                    color: parent.parent.selected ? root.bgColor : root.subtext
                                    font.family: root.fontFamily
                                    font.pixelSize: 11
                                    elide: Text.ElideRight
                                    width: parent.width
                                    visible: text.length > 0
                                }
                            }

                            MouseArea {
                                id: appMouse
                                anchors.fill: parent
                                hoverEnabled: true
                                cursorShape: Qt.PointingHandCursor
                                onClicked: {
                                    modelData.execute()
                                    root.launcherOpen = false
                                    root.launcherSearch = ""
                                    searchInput.text = ""
                                    root.selectedIndex = 0
                                }
                                onEntered: root.selectedIndex = index
                            }
                        }
                    }
                }

                // scroll to change volume (clock / volume modes only)
                MouseArea {
                    anchors.fill: parent
                    visible: root.pillMode === "clock" || root.pillMode === "volume"
                    acceptedButtons: Qt.NoButton
                    property int scrollAccum: 0
                    onWheel: event => {
                        scrollAccum += event.angleDelta.y
                        while (scrollAccum >= 120) { volUp.running = true; scrollAccum -= 120 }
                        while (scrollAccum <= -120) { volDown.running = true; scrollAccum += 120 }
                    }
                }

                Timer {
                    id: focusTimer
                    interval: 50
                    property int tries: 0
                    onTriggered: {
                        searchInput.forceActiveFocus()
                        tries++
                        if (!searchInput.activeFocus && tries < 6)
                            focusTimer.restart()
                        else
                            tries = 0
                    }
                }

                Connections {
                    target: root
                    function onLauncherOpenChanged() {
                        if (root.launcherOpen) {
                            root.volumeOsd = false
                            searchInput.text = ""
                            root.launcherSearch = ""
                            root.selectedIndex = 0
                            focusTimer.start()
                        }
                    }
                    function onSelectedIndexChanged() {
                        if (!root.launcherOpen) return
                        appListView.positionViewAtIndex(root.selectedIndex, ListView.Contain)
                    }
                }

            }

            // == portfolio widget — morphs between pill and expanded card (one element) ==
            Rectangle {
                id: portfolioCard
                visible: bar.isDp && root.portfolioValue > 0
                anchors.right: parent.right
                anchors.rightMargin: 17
                y: 6
                color: (!root.portfolioExpanded && pfOpen.containsMouse) ? root.surface : root.bgColor
                clip: true

                Behavior on color { ColorAnimation { duration: 120 } }

                // positions + a trailing cash row
                property var rows: {
                    var r = (root.portfolioPositions || []).slice()
                    r.push({ sym: "Cash", value: root.portfolioCash, dayPL: 0, totalPL: 0, isCash: true })
                    return r
                }
                property real listH: Math.min(rows.length * 27, 8 * 27)
                // only show the sparkline when there's actual variation (not a flat/closed line)
                property bool showSpark: {
                    var p = root.portfolioSpark || []
                    if (p.length < 2) return false
                    return (Math.max.apply(null, p) - Math.min.apply(null, p)) > 0.01
                }
                property real fullH: (showSpark ? 171 : 133) + listH

                // morph: pill (collapsedW x 40) <-> card (360 x fullH)
                property real collapsedW: pfCollapsed.implicitWidth + 28
                width: root.portfolioExpanded ? 360 : collapsedW
                height: root.portfolioExpanded ? fullH : 40
                radius: root.portfolioExpanded ? 16 : 20

                // 0..1 progress used to cross-fade pill <-> card content
                property real morphP: {
                    var d = 360 - collapsedW
                    return d > 0 ? Math.max(0, Math.min(1, (width - collapsedW) / d)) : 1
                }

                Behavior on width {
                    NumberAnimation { duration: 300; easing.bezierCurve: [0.16, 1, 0.3, 1, 1, 1] }
                }
                Behavior on height {
                    NumberAnimation { duration: 300; easing.bezierCurve: [0.16, 1, 0.3, 1, 1, 1] }
                }
                Behavior on radius {
                    NumberAnimation { duration: 300; easing.bezierCurve: [0.16, 1, 0.3, 1, 1, 1] }
                }

                property bool dayUp: root.portfolioChange >= 0
                property bool totalUp: root.portfolioTotalPL >= 0

                // opener — active only while collapsed (click the pill to expand)
                MouseArea {
                    id: pfOpen
                    anchors.fill: parent
                    enabled: !root.portfolioExpanded
                    hoverEnabled: true
                    cursorShape: Qt.PointingHandCursor
                    onClicked: root.portfolioExpanded = true
                }

                // collapsed face: value + daily % (fades out as the card opens)
                Row {
                    id: pfCollapsed
                    anchors.right: parent.right
                    anchors.rightMargin: 15
                    anchors.top: parent.top
                    height: 40
                    spacing: 9
                    opacity: 1 - Math.min(1, portfolioCard.morphP / 0.35)
                    visible: opacity > 0.01

                    Text {
                        anchors.verticalCenter: parent.verticalCenter
                        text: root.fmtMoney(root.portfolioValue)
                        color: root.textColor
                        font.family: root.fontFamily
                        font.pixelSize: 14
                        opacity: root.portfolioSample ? 0.55 : 1.0
                    }
                    Row {
                        anchors.verticalCenter: parent.verticalCenter
                        spacing: 4
                        visible: root.marketOpen
                        opacity: root.portfolioSample ? 0.55 : 1.0
                        Text {
                            anchors.verticalCenter: parent.verticalCenter
                            text: portfolioCard.dayUp ? "\u25b2" : "\u25bc"
                            color: portfolioCard.dayUp ? root.gainColor : root.mutedColor
                            font.family: root.fontFamily
                            font.pixelSize: 9
                        }
                        Text {
                            anchors.verticalCenter: parent.verticalCenter
                            text: Math.abs(root.portfolioChangePct).toFixed(2) + "%"
                            color: portfolioCard.dayUp ? root.gainColor : root.mutedColor
                            font.family: root.fontFamily
                            font.pixelSize: 13
                        }
                    }
                    Text {
                        anchors.verticalCenter: parent.verticalCenter
                        visible: !root.marketOpen
                        text: "\u2013"
                        color: root.subtext
                        font.family: root.fontFamily
                        font.pixelSize: 14
                    }
                }

                Column {
                    anchors.fill: parent
                    anchors.margins: 14
                    spacing: 8
                    // reveal content in the last part of the unfold so it doesn't squish
                    opacity: Math.max(0, (portfolioCard.morphP - 0.4) / 0.6)
                    visible: opacity > 0.01

                    // header: label + total value
                    Item {
                        width: parent.width
                        height: 22
                        Text {
                            anchors.left: parent.left
                            anchors.verticalCenter: parent.verticalCenter
                            text: root.portfolioSample ? "Portfolio (sample)" : "Portfolio"
                            color: root.subtext
                            font.family: root.fontFamily
                            font.pixelSize: 11
                        }
                        Text {
                            anchors.right: parent.right
                            anchors.verticalCenter: parent.verticalCenter
                            text: root.fmtMoney(root.portfolioValue)
                            color: root.textColor
                            font.family: root.fontFamily
                            font.pixelSize: 16
                            font.bold: true
                        }
                        // click the header to collapse
                        MouseArea {
                            anchors.fill: parent
                            anchors.margins: -14
                            cursorShape: Qt.PointingHandCursor
                            onClicked: root.portfolioExpanded = false
                        }
                    }

                    // day + total P&L
                    Item {
                        width: parent.width
                        height: 36

                        Column {
                            anchors.left: parent.left
                            anchors.verticalCenter: parent.verticalCenter
                            spacing: 2
                            Text {
                                text: "TODAY"
                                color: root.subtext
                                font.family: root.fontFamily
                                font.pixelSize: 9
                            }
                            Text {
                                text: root.marketOpen
                                    ? (portfolioCard.dayUp ? "\u25b2 " : "\u25bc ") + root.fmtMoney(root.portfolioChange) + "  " + Math.abs(root.portfolioChangePct).toFixed(2) + "%"
                                    : "market closed"
                                color: root.marketOpen ? (portfolioCard.dayUp ? root.gainColor : root.mutedColor) : root.subtext
                                font.family: root.fontFamily
                                font.pixelSize: 13
                            }
                        }

                        Column {
                            anchors.right: parent.right
                            anchors.verticalCenter: parent.verticalCenter
                            spacing: 2
                            Text {
                                anchors.right: parent.right
                                text: "TOTAL P&L"
                                color: root.subtext
                                font.family: root.fontFamily
                                font.pixelSize: 9
                            }
                            Text {
                                anchors.right: parent.right
                                text: (portfolioCard.totalUp ? "\u25b2 " : "\u25bc ") + root.fmtMoney(root.portfolioTotalPL)
                                color: portfolioCard.totalUp ? root.gainColor : root.mutedColor
                                font.family: root.fontFamily
                                font.pixelSize: 13
                            }
                        }
                    }

                    // intraday sparkline (hidden when flat / market closed)
                    Canvas {
                        id: sparkCanvas
                        width: parent.width
                        height: 30
                        visible: portfolioCard.showSpark
                        property var pts: root.portfolioSpark
                        onPtsChanged: requestPaint()
                        onVisibleChanged: if (visible) requestPaint()

                        onPaint: {
                            var ctx = getContext('2d')
                            ctx.reset()
                            var p = pts || []
                            var n = p.length
                            if (n < 2) return
                            var mn = Math.min.apply(null, p)
                            var mx = Math.max.apply(null, p)
                            var rng = (mx - mn) || 1
                            var pad = 3
                            var w = width, h = height
                            var up = p[n - 1] >= p[0]
                            var col = root.marketOpen ? (up ? root.gainColor : root.mutedColor) : root.subtext
                            ctx.beginPath()
                            for (var i = 0; i < n; i++) {
                                var x = (i / (n - 1)) * w
                                var y = h - pad - ((p[i] - mn) / rng) * (h - 2 * pad)
                                if (i === 0) ctx.moveTo(x, y)
                                else ctx.lineTo(x, y)
                            }
                            ctx.lineWidth = 1.5
                            ctx.lineJoin = "round"
                            ctx.strokeStyle = col
                            ctx.stroke()
                        }
                    }

                    // column headers (aligned to the row columns below)
                    Item {
                        width: parent.width
                        height: 14
                        Row {
                            anchors.right: parent.right
                            anchors.verticalCenter: parent.verticalCenter
                            spacing: 10
                            Text {
                                width: 66; horizontalAlignment: Text.AlignRight
                                text: "VALUE"; color: root.subtext
                                font.family: root.fontFamily; font.pixelSize: 9
                            }
                            Text {
                                width: 66; horizontalAlignment: Text.AlignRight
                                text: "DAY"; color: root.subtext
                                font.family: root.fontFamily; font.pixelSize: 9
                            }
                            Text {
                                width: 72; horizontalAlignment: Text.AlignRight
                                text: "TOTAL"; color: root.subtext
                                font.family: root.fontFamily; font.pixelSize: 9
                            }
                        }
                    }

                    Rectangle {
                        width: parent.width
                        height: 1
                        color: root.surface
                    }

                    // scrollable holdings (positions + cash): value | day P&L | total P&L
                    ListView {
                        width: parent.width
                        height: portfolioCard.listH
                        clip: true
                        model: portfolioCard.rows
                        boundsBehavior: Flickable.StopAtBounds

                        delegate: Item {
                            required property var modelData
                            width: ListView.view.width
                            height: 27

                            property real dpl: modelData.dayPL || 0
                            property real tpl: modelData.totalPL || 0
                            function plColor(v) {
                                return v > 0 ? root.gainColor : v < 0 ? root.mutedColor : root.subtext
                            }
                            function plText(v) {
                                return (v >= 0 ? "+" : "\u2212") + root.fmtMoney(v)
                            }

                            Text {
                                anchors.left: parent.left
                                anchors.verticalCenter: parent.verticalCenter
                                text: modelData.sym || ""
                                color: root.textColor
                                font.family: root.fontFamily
                                font.pixelSize: 13
                            }

                            Row {
                                anchors.right: parent.right
                                anchors.verticalCenter: parent.verticalCenter
                                spacing: 10

                                // market value
                                Text {
                                    anchors.verticalCenter: parent.verticalCenter
                                    width: 66
                                    horizontalAlignment: Text.AlignRight
                                    text: root.fmtMoney(modelData.value)
                                    color: root.subtext
                                    font.family: root.fontFamily
                                    font.pixelSize: 12
                                }

                                // day P&L ($) — neutral dash when closed / cash
                                Text {
                                    anchors.verticalCenter: parent.verticalCenter
                                    width: 66
                                    horizontalAlignment: Text.AlignRight
                                    text: (modelData.isCash || !root.marketOpen) ? "\u2013" : parent.parent.plText(parent.parent.dpl)
                                    color: (modelData.isCash || !root.marketOpen) ? root.subtext : parent.parent.plColor(parent.parent.dpl)
                                    font.family: root.fontFamily
                                    font.pixelSize: 12
                                }

                                // total P&L ($) — colored by sign
                                Text {
                                    anchors.verticalCenter: parent.verticalCenter
                                    width: 72
                                    horizontalAlignment: Text.AlignRight
                                    text: modelData.isCash ? "\u2013" : parent.parent.plText(parent.parent.tpl)
                                    color: modelData.isCash ? root.subtext : parent.parent.plColor(parent.parent.tpl)
                                    font.family: root.fontFamily
                                    font.pixelSize: 12
                                }
                            }
                        }
                    }
                }
            }
        }
    }

}
