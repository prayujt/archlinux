import Quickshell
import Quickshell.Hyprland
import Quickshell.Io
import Quickshell.Wayland
import Quickshell.Services.Notifications
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
    // true briefly while the OSD morphs open, so the fill bar snaps to the real
    // value instead of animating up from a stale/zero value
    property bool sonosVolSettling: false
    property string sonosState: "STOPPED"
    // settable so play/pause can update the UI optimistically before the poll catches up
    property bool sonosPlaying: false
    // while locked, the poller won't overwrite sonosPlaying (avoids flip-back after a manual toggle)
    property bool sonosPlayLock: false
    Timer { id: sonosPlayLockTimer; interval: 2500; onTriggered: root.sonosPlayLock = false }
    function sonosTogglePlay() {
        root.sonosPlaying = !root.sonosPlaying
        root.sonosPlayLock = true
        sonosPlayLockTimer.restart()
        sonosToggle.running = true
    }
    property bool sonosHasTrack: (sonosState === "PLAYING" || sonosState === "PAUSED_PLAYBACK") && sonosTitle.length > 0
    property bool mediaExpanded: false
    // true briefly when the media card opens, so the progress bar snaps to the
    // current position instead of crawling there
    property bool mediaSettling: false
    onMediaExpandedChanged: if (mediaExpanded) { mediaSettling = true; mediaSettleTimer.restart() }
    Timer { id: mediaSettleTimer; interval: 320; onTriggered: root.mediaSettling = false }

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
    property bool newsExpanded: false
    property int newsSelected: 0

    // -- telegram (ray) chat state --
    property bool telegramOpen: false
    property bool telegramReady: false
    property string telegramError: ""
    property bool tgTyping: false
    property var tgSeen: ({})
    // -- unified notification center (desktop notifications + ray messages) --
    property bool notifOpen: false
    property int notifSelected: 0
    // newest first: { kind, app, summary, body, time, ref } — see notifModel below
    // transient toast (latest notification) shown under the pill
    property string notifToastApp: ""
    property string notifToastText: ""
    property bool notifToastShow: false
    Timer { id: notifToastTimer; interval: 10000; onTriggered: root.notifToastShow = false }

    ListModel { id: notifModel }

    function oneLine(t) { return (t || "").replace(/\s+/g, " ").trim() }
    function addNotifItem(entry) {
        entry.time = Date.now()
        notifModel.insert(0, entry)
        while (notifModel.count > 50) notifModel.remove(notifModel.count - 1)
    }
    function showToast(app, text) {
        root.notifToastApp = app
        root.notifToastText = root.oneLine(text)
        root.notifToastShow = true
        notifToastTimer.restart()
    }
    function removeNotifAt(i) {
        if (i < 0 || i >= notifModel.count) return
        var e = notifModel.get(i)
        if (e && e.ref) { try { e.ref.tracked = false } catch(err) {} }
        notifModel.remove(i)   // fires ListView remove/displaced transitions
        if (root.notifSelected >= notifModel.count)
            root.notifSelected = Math.max(0, notifModel.count - 1)
    }
    function clearNotifs() {
        for (var i = 0; i < notifModel.count; i++) {
            var e = notifModel.get(i)
            if (e && e.ref) { try { e.ref.tracked = false } catch(err) {} }
        }
        notifModel.clear()
        root.notifSelected = 0
        root.notifOpen = false
    }
    function activateNotif(i) {
        if (i < 0 || i >= notifModel.count) return
        var e = notifModel.get(i)
        if (!e) return
        if (e.kind === "ray") {
            root.telegramOpen = true       // open the chat, consume the entry
            removeNotifAt(i)
            return
        }
        // app notification: invoke its default action if one exists, then clear it
        if (e.ref) {
            try {
                var acts = e.ref.actions
                if (acts && acts.length > 0) {
                    var chosen = acts[0]
                    for (var k = 0; k < acts.length; k++) {
                        if (acts[k].identifier === "default") { chosen = acts[k]; break }
                    }
                    chosen.invoke()
                }
            } catch (err) {}
        }
        removeNotifAt(i)
        root.notifOpen = false
    }
    function relTime(ms) {
        var d = Math.max(0, Date.now() - ms)
        var s = Math.floor(d / 1000)
        if (s < 10) return "now"
        if (s < 60) return s + "s"
        var m = Math.floor(s / 60)
        if (m < 60) return m + "m"
        var h = Math.floor(m / 60)
        if (h < 24) return h + "h"
        return Math.floor(h / 24) + "d"
    }

    // become the desktop notification daemon and feed the center + toast
    NotificationServer {
        id: notifServer
        keepOnReload: true
        bodySupported: true
        actionsSupported: true
        imageSupported: true
        onNotification: n => {
            n.tracked = true
            root.addNotifItem({ kind: "app", app: n.appName || "app",
                                summary: root.oneLine(n.summary), body: root.oneLine(n.body), ref: n })
            root.showToast(n.appName || "notification", n.summary || n.body || "")
        }
    }

    // any keyboard-driven panel open on the center pill
    readonly property bool anyPanel: launcherOpen || newsExpanded || telegramOpen || notifOpen || (mediaExpanded && sonosHasTrack)
    // briefly grab keyboard focus on open (Exclusive) then relax to OnDemand so the
    // rest of the desktop stays usable while the panel keeps focus until you click away
    property bool kbGrab: false
    onAnyPanelChanged: if (anyPanel) { kbGrab = true; kbGrabTimer.restart(); warpCursorToPanel() }
    Timer { id: kbGrabTimer; interval: 450; onTriggered: root.kbGrab = false }
    // snap the volume fill while the OSD morphs in, then re-enable smooth animation
    onSonosVolOsdChanged: if (sonosVolOsd) { sonosVolSettling = true; volSettleTimer.restart() }
    Timer { id: volSettleTimer; interval: 320; onTriggered: root.sonosVolSettling = false }

    // warp the cursor onto the open panel so focus-follows-mouse doesn't yank focus
    // back to whatever window was under the cursor when we relax to OnDemand
    Process { id: cursorWarpProc }
    function warpCursorToPanel() {
        var s = null
        for (var i = 0; i < Quickshell.screens.length; i++)
            if (Quickshell.screens[i].name === "DP-3") s = Quickshell.screens[i]
        if (!s) return
        var cx = newsExpanded ? (s.x + s.width - 380) : (s.x + s.width / 2)
        var cy = s.y + 70
        cursorWarpProc.command = ["hyprctl", "dispatch", "movecursor",
                                  Math.round(cx).toString(), Math.round(cy).toString()]
        cursorWarpProc.running = true
    }

    property string pillMode: launcherOpen ? "launcher"
        : telegramOpen ? "telegram"
        : notifOpen ? "notif"
        : (mediaExpanded && sonosHasTrack) ? "media"
        : sonosVolOsd ? "sonosvol"
        : volumeOsd ? "volume"
        : "clock"

    // collapse media card if track stops
    onSonosHasTrackChanged: if (!sonosHasTrack) mediaExpanded = false
    // portfolio and news cards are mutually exclusive
    onPortfolioExpandedChanged: if (portfolioExpanded) newsExpanded = false
    onNewsExpandedChanged: if (newsExpanded) portfolioExpanded = false
    // telegram takes over the center pill; close the launcher when it opens
    onTelegramOpenChanged: {
        if (telegramOpen) { launcherOpen = false; notifOpen = false; notifToastShow = false }
    }
    onNotifOpenChanged: if (notifOpen) { launcherOpen = false; telegramOpen = false; mediaExpanded = false }
    onLauncherOpenChanged: if (launcherOpen) telegramOpen = false

    // telegram message history (out=true is us, out=false is ray)
    ListModel { id: tgModel }

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
        // NYT US Politics + Economy (hard news only; see scripts/news-fetch to change source)
        command: ["/home/prayuj/.nix-profile/bin/python3", "/home/prayuj/.config/quickshell/scripts/news-fetch"]
        stdout: SplitParser {
            splitMarker: "\n"
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
                if (data.trim() === "toggle") root.newsExpanded = !root.newsExpanded
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

    // -- notification center toggle listener (FIFO, MOD+SHIFT+I) --
    Process {
        id: notifToggleProc
        running: true
        command: ["sh", "-c", "rm -f /tmp/qs-notif-toggle; mkfifo /tmp/qs-notif-toggle; while true; do cat /tmp/qs-notif-toggle; done"]
        stdout: SplitParser {
            onRead: data => {
                if (data.trim() === "toggle") root.notifOpen = !root.notifOpen
            }
        }
    }

    // -- portfolio card toggle listener (FIFO) --  // portfolio widget temporarily hidden
    /*
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
    */

    // -- media card toggle listener (FIFO) — only meaningful while a track is playing --
    Process {
        id: mediaToggleProc
        running: true
        command: ["sh", "-c", "rm -f /tmp/qs-media-toggle; mkfifo /tmp/qs-media-toggle; while true; do cat /tmp/qs-media-toggle; done"]
        stdout: SplitParser {
            onRead: data => {
                if (data.trim() === "toggle" && root.sonosHasTrack)
                    root.mediaExpanded = !root.mediaExpanded
            }
        }
    }

    // -- telegram (ray) daemon: streams history + live messages, reads /tmp/qs-tg-send --
    Process {
        id: telegramProc
        running: true
        command: ["/home/prayuj/.config/quickshell/.venv-tg/bin/python",
                  "/home/prayuj/.config/quickshell/scripts/telegram-ray"]
        stdout: SplitParser {
            splitMarker: "\n"
            onRead: data => {
                try {
                    let o = JSON.parse(data.trim())
                    if (o.type === "history") {
                        tgModel.clear()
                        root.tgSeen = ({})
                        for (let i = 0; i < o.messages.length; i++) {
                            let m = o.messages[i]
                            if (m.id !== undefined) root.tgSeen[m.id] = true
                            tgModel.append({ mid: m.id === undefined ? -1 : m.id, out: m.out, text: m.text })
                        }
                        root.telegramReady = true
                    } else if (o.type === "msg") {
                        root.tgTyping = false
                        // dedupe by message id so duplicates never spam the view
                        if (o.id === undefined || !root.tgSeen[o.id]) {
                            if (o.id !== undefined) root.tgSeen[o.id] = true
                            tgModel.append({ mid: o.id === undefined ? -1 : o.id, out: o.out, text: o.text })
                        }
                        // add ray replies to the notification center; toast when chat is closed
                        if (!o.out && o.text && o.text.length > 0) {
                            root.addNotifItem({ kind: "ray", app: "ray", summary: root.oneLine(o.text), body: "" })
                            if (!root.telegramOpen) root.showToast("ray", o.text)
                        }
                    } else if (o.type === "edit") {
                        // ray is streaming: update the existing bubble in place
                        root.tgTyping = false
                        let found = false
                        for (let j = tgModel.count - 1; j >= 0; j--) {
                            if (tgModel.get(j).mid === o.id) {
                                tgModel.setProperty(j, "text", o.text)
                                found = true
                                break
                            }
                        }
                        if (!found) {
                            if (o.id !== undefined) root.tgSeen[o.id] = true
                            tgModel.append({ mid: o.id === undefined ? -1 : o.id, out: o.out, text: o.text })
                        }
                        // keep the toast in sync while ray streams, if it's showing
                        if (!o.out && !root.telegramOpen && root.notifToastShow && o.text) {
                            root.notifToastText = root.oneLine(o.text)
                            notifToastTimer.restart()
                        }
                    } else if (o.type === "typing") {
                        root.tgTyping = o.on === true
                    } else if (o.type === "status") {
                        root.telegramReady = o.ok === true
                        root.telegramError = o.error || ""
                    }
                } catch(e) {}
            }
        }
    }

    // restart the daemon if it dies
    Timer {
        interval: 15000; running: true; repeat: true
        onTriggered: if (!telegramProc.running) telegramProc.running = true
    }

    // -- telegram toggle listener (FIFO, MOD+T) --
    Process {
        id: telegramToggleProc
        running: true
        command: ["sh", "-c", "rm -f /tmp/qs-telegram-toggle; mkfifo /tmp/qs-telegram-toggle; while true; do cat /tmp/qs-telegram-toggle; done"]
        stdout: SplitParser {
            onRead: data => {
                if (data.trim() === "toggle") root.telegramOpen = !root.telegramOpen
            }
        }
    }

    // -- send a message to ray (writes one line to the daemon's FIFO) --
    // `timeout` guarantees the write can never block/pile up if the daemon is down
    Process { id: telegramSendProc }
    function telegramSend(text) {
        if (!text || text.length === 0) return
        telegramSendProc.command = ["timeout", "2", "sh", "-c", "printf '%s\\n' \"$1\" > /tmp/qs-tg-send", "sh", text]
        telegramSendProc.running = true
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
                    if (!root.sonosPlayLock)
                        root.sonosPlaying = (m.state === "PLAYING")
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

    // -- schwab portfolio: read-only poll (sample data until authorized) --  // portfolio widget temporarily hidden
    /*
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
    */

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
            // brief Exclusive grab on open (auto-focus) then OnDemand (releasable,
            // doesn't monopolize the keyboard — other apps stay usable, click to refocus)
            WlrLayershell.keyboardFocus: !(isDp && root.anyPanel) ? WlrKeyboardFocus.None
                : (root.kbGrab ? WlrKeyboardFocus.Exclusive : WlrKeyboardFocus.OnDemand)

            mask: Region {
                Region { item: barContent }
                Region { item: pillBg }
                // Region { item: portfolioCard }  // portfolio widget temporarily hidden
                Region { item: newsCard }
                Region { item: tgNotif }
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
                    if (root.pillMode === "telegram") return 430
                    if (root.pillMode === "notif") return 430
                    if (root.pillMode === "media") return 390
                    if (root.pillMode === "volume") return 220
                    if (root.pillMode === "sonosvol") return root.sonosHasTrack ? 300 : 240
                    // shrink to fit the title, capped at 300 (long titles stay elided)
                    if (compactMedia) return Math.max(120, Math.min(300, 79 + npTitle.implicitWidth))
                    return 90
                }
                function modeH() {
                    if (root.pillMode === "launcher") return 500
                    if (root.pillMode === "telegram") return 360
                    if (root.pillMode === "notif") return 380
                    if (root.pillMode === "media") return 150
                    if (root.pillMode === "sonosvol") return 68
                    return 40
                }

                width: modeW()
                height: modeH()
                color: root.bgColor
                radius: (root.pillMode === "launcher" || root.pillMode === "media" || root.pillMode === "telegram" || root.pillMode === "notif") ? 16 : 20

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
                        NumberAnimation { duration: 120; easing.type: Easing.OutCubic }
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
                                    enabled: !root.sonosVolSettling
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

                    // reveal only once the pill has mostly grown, so the compact chip
                    // (which fades out fast) is gone before the card content appears —
                    // avoids the two layouts overlapping/flickering mid-morph
                    property real reveal: root.pillMode === "media"
                        ? Math.max(0, Math.min(1, (pillBg.height - 96) / (140 - 96)))
                        : 0
                    opacity: reveal
                    visible: opacity > 0.01

                    Behavior on opacity {
                        NumberAnimation { duration: 90; easing.type: Easing.OutCubic }
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
                                    onClicked: root.sonosTogglePlay()
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
                                        enabled: !root.mediaSettling
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

                    // keyboard control while the media card is open:
                    // Space = play/pause, Ctrl+L = next, Ctrl+H = previous, Esc = close
                    Item {
                        id: mediaKeys
                        anchors.fill: parent
                        focus: root.pillMode === "media"
                        Keys.onPressed: event => {
                            if (root.pillMode !== "media") return
                            if (event.key === Qt.Key_Escape) {
                                root.mediaExpanded = false; event.accepted = true
                            } else if (event.key === Qt.Key_Space) {
                                root.sonosTogglePlay(); event.accepted = true
                            } else if (event.modifiers & Qt.ControlModifier) {
                                if (event.key === Qt.Key_L) { sonosNext.running = true; event.accepted = true }
                                else if (event.key === Qt.Key_H) { sonosPrev.running = true; event.accepted = true }
                            }
                        }
                    }

                    Timer {
                        id: mediaFocusTimer
                        interval: 60
                        property int tries: 0
                        onTriggered: {
                            mediaKeys.forceActiveFocus()
                            tries++
                            if (!mediaKeys.activeFocus && tries < 6) mediaFocusTimer.restart()
                            else tries = 0
                        }
                    }

                    Connections {
                        target: root
                        function onMediaExpandedChanged() {
                            if (root.mediaExpanded) mediaFocusTimer.start()
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

                // -- telegram (ray) chat face --
                Item {
                    id: tgFace
                    anchors.fill: parent
                    anchors.margins: 12
                    opacity: root.pillMode === "telegram" ? pillBg.morphCloseness : 0
                    visible: opacity > 0.01

                    // header: status dot + "ray"
                    Item {
                        id: tgHeader
                        anchors.left: parent.left
                        anchors.right: parent.right
                        anchors.top: parent.top
                        height: 20

                        Rectangle {
                            id: tgDot
                            anchors.left: parent.left
                            anchors.verticalCenter: parent.verticalCenter
                            width: 7; height: 7; radius: 3.5
                            color: root.telegramReady ? root.gainColor : root.mutedColor
                        }
                        Text {
                            anchors.left: tgDot.right
                            anchors.leftMargin: 8
                            anchors.verticalCenter: parent.verticalCenter
                            text: "ray"
                            color: root.textColor
                            font.family: root.fontFamily
                            font.pixelSize: 13
                            font.bold: true
                        }
                        Text {
                            anchors.right: parent.right
                            anchors.verticalCenter: parent.verticalCenter
                            text: root.telegramReady ? "" : (root.telegramError.length ? root.telegramError : "connecting\u2026")
                            color: root.subtext
                            font.family: root.fontFamily
                            font.pixelSize: 9
                            elide: Text.ElideLeft
                            width: 180
                            horizontalAlignment: Text.AlignRight
                        }
                    }

                    // input box (bottom)
                    Rectangle {
                        id: tgInputBox
                        anchors.left: parent.left
                        anchors.right: parent.right
                        anchors.bottom: parent.bottom
                        height: 34
                        radius: 10
                        color: root.surface

                        TextInput {
                            id: tgInput
                            anchors.fill: parent
                            anchors.leftMargin: 12
                            anchors.rightMargin: 12
                            verticalAlignment: Text.AlignVCenter
                            color: root.textColor
                            font.family: root.fontFamily
                            font.pixelSize: 13
                            clip: true

                            Keys.onEscapePressed: root.telegramOpen = false
                            Keys.onReturnPressed: {
                                let t = tgInput.text
                                if (t.trim().length > 0) {
                                    root.telegramSend(t)
                                    tgInput.text = ""
                                }
                            }
                            Keys.onPressed: event => {
                                if (event.modifiers & Qt.ControlModifier) {
                                    if (event.key === Qt.Key_J) { tgList.flick(0, -700); event.accepted = true }
                                    else if (event.key === Qt.Key_K) { tgList.flick(0, 700); event.accepted = true }
                                }
                            }
                        }
                        Text {
                            anchors.left: parent.left
                            anchors.leftMargin: 12
                            anchors.verticalCenter: parent.verticalCenter
                            text: "message ray\u2026"
                            color: root.subtext
                            font.family: root.fontFamily
                            font.pixelSize: 13
                            visible: tgInput.text.length === 0
                        }
                    }

                    // message list
                    ListView {
                        id: tgList
                        anchors.left: parent.left
                        anchors.right: parent.right
                        anchors.top: tgHeader.bottom
                        anchors.topMargin: 8
                        anchors.bottom: tgInputBox.top
                        anchors.bottomMargin: 8
                        clip: true
                        spacing: 5
                        model: tgModel
                        boundsBehavior: Flickable.StopAtBounds
                        cacheBuffer: 800

                        onCountChanged: Qt.callLater(positionViewAtEnd)

                        // typing indicator (animated dots) shown while ray is composing
                        footer: Item {
                            width: tgList.width
                            height: root.tgTyping ? 26 : 0
                            visible: root.tgTyping
                            onVisibleChanged: if (visible) Qt.callLater(tgList.positionViewAtEnd)

                            Rectangle {
                                anchors.left: parent.left
                                anchors.top: parent.top
                                anchors.topMargin: 3
                                width: 44
                                height: 20
                                radius: 10
                                color: root.surface

                                Row {
                                    anchors.centerIn: parent
                                    spacing: 4
                                    Repeater {
                                        model: 3
                                        Rectangle {
                                            required property int index
                                            width: 5; height: 5; radius: 2.5
                                            color: root.subtext
                                            SequentialAnimation on opacity {
                                                running: root.tgTyping
                                                loops: Animation.Infinite
                                                PauseAnimation { duration: index * 160 }
                                                NumberAnimation { to: 1.0; duration: 300; easing.type: Easing.InOutSine }
                                                NumberAnimation { to: 0.3; duration: 300; easing.type: Easing.InOutSine }
                                                PauseAnimation { duration: (2 - index) * 160 }
                                            }
                                        }
                                    }
                                }
                            }
                        }

                        delegate: Item {
                            width: tgList.width
                            implicitHeight: bubble.height + 4
                            height: bubble.height + 4

                            Rectangle {
                                id: bubble
                                x: model.out ? parent.width - width : 0
                                width: Math.min(tgList.width * 0.8, msgText.implicitWidth + 22)
                                height: msgText.implicitHeight + 14
                                radius: 10
                                color: model.out ? root.accent : root.surface

                                Text {
                                    id: msgText
                                    x: 11
                                    y: 7
                                    width: Math.min(implicitWidth, tgList.width * 0.8 - 22)
                                    text: model.text
                                    wrapMode: Text.Wrap
                                    color: model.out ? root.bgColor : root.textColor
                                    font.family: root.fontFamily
                                    font.pixelSize: 12
                                }
                            }
                        }
                    }

                    Timer {
                        id: tgFocusTimer
                        interval: 60
                        property int tries: 0
                        onTriggered: {
                            tgInput.forceActiveFocus()
                            tries++
                            if (!tgInput.activeFocus && tries < 6) tgFocusTimer.restart()
                            else tries = 0
                        }
                    }

                    Connections {
                        target: root
                        function onTelegramOpenChanged() {
                            if (root.telegramOpen) {
                                tgInput.text = ""
                                tgFocusTimer.start()
                                tgList.positionViewAtEnd()
                            }
                        }
                    }
                }

                // -- notification center face --
                Item {
                    id: notifFace
                    anchors.fill: parent
                    anchors.margins: 12
                    opacity: root.pillMode === "notif" ? pillBg.morphCloseness : 0
                    visible: opacity > 0.01

                    Item {
                        id: notifHeader
                        anchors.left: parent.left
                        anchors.right: parent.right
                        anchors.top: parent.top
                        height: 22
                        Text {
                            anchors.left: parent.left
                            anchors.verticalCenter: parent.verticalCenter
                            text: "Notifications"
                            color: root.subtext
                            font.family: root.fontFamily
                            font.pixelSize: 11
                        }
                        Text {
                            id: notifClear
                            anchors.right: parent.right
                            anchors.verticalCenter: parent.verticalCenter
                            text: notifModel.count > 0 ? "clear all" : ""
                            color: clearMouse.containsMouse ? root.textColor : root.subtext
                            font.family: root.fontFamily
                            font.pixelSize: 10
                            MouseArea {
                                id: clearMouse
                                anchors.fill: parent
                                anchors.margins: -6
                                hoverEnabled: true
                                cursorShape: Qt.PointingHandCursor
                                onClicked: root.clearNotifs()
                            }
                        }
                    }

                    Rectangle {
                        id: notifDiv
                        anchors.top: notifHeader.bottom
                        anchors.topMargin: 7
                        anchors.left: parent.left
                        anchors.right: parent.right
                        height: 1
                        color: root.surface
                    }

                    Text {
                        anchors.centerIn: parent
                        visible: notifModel.count === 0
                        text: "No notifications"
                        color: root.subtext
                        font.family: root.fontFamily
                        font.pixelSize: 12
                    }

                    ListView {
                        id: notifList
                        anchors.top: notifDiv.bottom
                        anchors.topMargin: 6
                        anchors.left: parent.left
                        anchors.right: parent.right
                        anchors.bottom: parent.bottom
                        clip: true
                        spacing: 4
                        model: notifModel
                        currentIndex: root.notifSelected
                        boundsBehavior: Flickable.StopAtBounds
                        cacheBuffer: 600

                        // clean card in/out morph
                        add: Transition {
                            NumberAnimation { property: "opacity"; from: 0; to: 1; duration: 160; easing.type: Easing.OutCubic }
                        }
                        remove: Transition {
                            NumberAnimation { property: "opacity"; to: 0; duration: 150; easing.type: Easing.OutCubic }
                            NumberAnimation { property: "height"; to: 0; duration: 190; easing.type: Easing.OutCubic }
                        }
                        removeDisplaced: Transition {
                            NumberAnimation { properties: "y"; duration: 190; easing.type: Easing.OutCubic }
                        }
                        displaced: Transition {
                            NumberAnimation { properties: "y"; duration: 190; easing.type: Easing.OutCubic }
                        }

                        delegate: Rectangle {
                            required property var model
                            required property int index
                            property bool selected: index === root.notifSelected
                            width: notifList.width
                            height: 54
                            radius: 8
                            color: selected ? root.accent : cardMouse.containsMouse ? root.surface : "transparent"
                            Behavior on color { ColorAnimation { duration: 90 } }

                            Column {
                                anchors.left: parent.left
                                anchors.leftMargin: 11
                                anchors.right: parent.right
                                anchors.rightMargin: 30
                                anchors.verticalCenter: parent.verticalCenter
                                spacing: 3

                                Row {
                                    width: parent.width
                                    spacing: 7
                                    Rectangle {
                                        anchors.verticalCenter: parent.verticalCenter
                                        width: 6; height: 6; radius: 3
                                        color: model.kind === "ray" ? root.clockColor : root.accent
                                    }
                                    Text {
                                        anchors.verticalCenter: parent.verticalCenter
                                        text: model.app || "app"
                                        color: parent.parent.parent.selected ? root.bgColor : root.textColor
                                        font.family: root.fontFamily
                                        font.pixelSize: 11
                                        font.bold: true
                                    }
                                    Text {
                                        anchors.verticalCenter: parent.verticalCenter
                                        text: root.relTime(model.time)
                                        color: parent.parent.parent.selected ? root.bgColor : root.subtext
                                        font.family: root.fontFamily
                                        font.pixelSize: 9
                                    }
                                }

                                Text {
                                    width: parent.width
                                    text: (model.summary || "") + (model.body ? " \u2014 " + model.body : "")
                                    color: parent.parent.selected ? root.bgColor : root.subtext
                                    font.family: root.fontFamily
                                    font.pixelSize: 12
                                    elide: Text.ElideRight
                                    maximumLineCount: 2
                                    wrapMode: Text.Wrap
                                }
                            }

                            Text {
                                anchors.right: parent.right
                                anchors.rightMargin: 10
                                anchors.top: parent.top
                                anchors.topMargin: 8
                                text: "\u00d7"
                                color: parent.selected ? root.bgColor : root.subtext
                                font.family: root.fontFamily
                                font.pixelSize: 13
                                MouseArea {
                                    anchors.fill: parent
                                    anchors.margins: -6
                                    cursorShape: Qt.PointingHandCursor
                                    onClicked: root.removeNotifAt(index)
                                }
                            }

                            MouseArea {
                                id: cardMouse
                                anchors.fill: parent
                                anchors.rightMargin: 26
                                hoverEnabled: true
                                cursorShape: Qt.PointingHandCursor
                                onEntered: root.notifSelected = index
                                onClicked: root.activateNotif(index)
                            }
                        }
                    }

                    // keyboard: Ctrl+J/K select, Enter open, D/Backspace dismiss, Esc close
                    Item {
                        id: notifKeys
                        anchors.fill: parent
                        focus: root.pillMode === "notif"
                        Keys.onPressed: event => {
                            if (root.pillMode !== "notif") return
                            if (event.key === Qt.Key_Escape) { root.notifOpen = false; event.accepted = true; return }
                            if (event.key === Qt.Key_Return || event.key === Qt.Key_Enter) {
                                root.activateNotif(root.notifSelected); event.accepted = true; return
                            }
                            if (event.key === Qt.Key_D || event.key === Qt.Key_Backspace) {
                                root.removeNotifAt(root.notifSelected); event.accepted = true; return
                            }
                            var ctrl = (event.modifiers & Qt.ControlModifier)
                            var down = event.key === Qt.Key_Down || (ctrl && event.key === Qt.Key_J)
                            var up = event.key === Qt.Key_Up || (ctrl && event.key === Qt.Key_K)
                            if (down) { if (root.notifSelected < notifModel.count - 1) root.notifSelected++; event.accepted = true }
                            else if (up) { if (root.notifSelected > 0) root.notifSelected--; event.accepted = true }
                        }
                    }

                    Timer {
                        id: notifFocusTimer
                        interval: 60
                        property int tries: 0
                        onTriggered: {
                            notifKeys.forceActiveFocus()
                            tries++
                            if (!notifKeys.activeFocus && tries < 6) notifFocusTimer.restart()
                            else tries = 0
                        }
                    }

                    Connections {
                        target: root
                        function onNotifOpenChanged() {
                            if (root.notifOpen) { root.notifSelected = 0; notifFocusTimer.start() }
                        }
                        function onNotifSelectedChanged() {
                            if (root.notifOpen) notifList.positionViewAtIndex(root.notifSelected, ListView.Contain)
                        }
                    }
                }

                // scroll to change volume (clock / volume modes only)
                MouseArea {
                    anchors.fill: parent
                    visible: root.pillMode === "clock" || root.pillMode === "volume"
                    acceptedButtons: Qt.NoButton
                    // show the pointer over the clickable now-playing chip
                    cursorShape: pillBg.compactMedia ? Qt.PointingHandCursor : Qt.ArrowCursor
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

            // == notification toast (below the pill) for any incoming notification ==
            Rectangle {
                id: tgNotif
                // hide only when a center-pill panel would overlap; news/portfolio (right side) are fine
                readonly property bool canShow: root.notifToastShow
                    && !(root.launcherOpen || root.telegramOpen || root.notifOpen
                         || (root.mediaExpanded && root.sonosHasTrack))
                anchors.horizontalCenter: parent.horizontalCenter
                y: pillBg.y + pillBg.height + 8
                width: Math.min(400, notifRow.implicitWidth + 28)
                // morph down from the pill: grow height in/out
                height: canShow ? 36 : 0
                visible: bar.isDp && height > 0.5
                radius: Math.min(18, height / 2)
                color: notifMouse.containsMouse ? root.surface : root.bgColor
                clip: true

                Behavior on height {
                    NumberAnimation { duration: 240; easing.bezierCurve: [0.16, 1, 0.3, 1, 1, 1] }
                }
                opacity: canShow ? 1 : 0
                Behavior on opacity { NumberAnimation { duration: 200; easing.type: Easing.OutCubic } }
                Behavior on color { ColorAnimation { duration: 120 } }

                Row {
                    id: notifRow
                    anchors.left: parent.left
                    anchors.leftMargin: 13
                    anchors.right: parent.right
                    anchors.rightMargin: 13
                    anchors.verticalCenter: parent.verticalCenter
                    spacing: 8

                    Rectangle {
                        anchors.verticalCenter: parent.verticalCenter
                        width: 8; height: 8; radius: 4
                        color: root.mutedColor
                        SequentialAnimation on opacity {
                            running: tgNotif.canShow
                            loops: Animation.Infinite
                            NumberAnimation { to: 0.3; duration: 550; easing.type: Easing.InOutSine }
                            NumberAnimation { to: 1.0; duration: 550; easing.type: Easing.InOutSine }
                        }
                    }
                    Text {
                        anchors.verticalCenter: parent.verticalCenter
                        text: root.notifToastApp
                        color: root.textColor
                        font.family: root.fontFamily
                        font.pixelSize: 12
                        font.bold: true
                    }
                    Text {
                        anchors.verticalCenter: parent.verticalCenter
                        width: Math.min(implicitWidth, 300)
                        text: root.notifToastText
                        color: root.subtext
                        font.family: root.fontFamily
                        font.pixelSize: 12
                        maximumLineCount: 1
                        elide: Text.ElideRight
                    }
                }

                MouseArea {
                    id: notifMouse
                    anchors.fill: parent
                    hoverEnabled: true
                    cursorShape: Qt.PointingHandCursor
                    onClicked: {
                        root.notifToastShow = false
                        if (root.notifToastApp === "ray") root.telegramOpen = true
                        else root.notifOpen = true
                    }
                }
            }

            // == news widget — morphs between ticker pill and expanded headlines card ==
            Rectangle {
                id: newsCard
                visible: bar.isDp
                property bool newsLoaded: root.headlines.length > 0
                anchors.right: parent.right
                // sit in the corner (replaces the hidden portfolio pill)
                anchors.rightMargin: 17
                y: 6
                color: (!root.newsExpanded && newsOpen.containsMouse) ? root.surface : root.bgColor
                clip: true
                opacity: 1
                Behavior on opacity { NumberAnimation { duration: 200; easing.type: Easing.OutCubic } }
                Behavior on color { ColorAnimation { duration: 120 } }

                property int newsIndex: 0
                property real scrollTo: Math.min(0, tickerClip.width - tickerText.implicitWidth - 4)
                // scroll time for this headline; short ones scroll little/none
                property int scrollDur: Math.max(1, Math.round(Math.abs(scrollTo) * 22))
                // pad the hold so short headlines linger a minimum time before swapping
                property int holdPad: Math.max(1100, 3400 - scrollDur)
                property real collapsedW: 380
                property real listH: Math.min(root.headlines.length * 46, 6 * 46)
                property real fullH: 67 + listH

                // morph: ticker (collapsedW x 40) <-> card (440 x fullH)
                width: root.newsExpanded ? 440 : collapsedW
                height: root.newsExpanded ? fullH : 40
                radius: root.newsExpanded ? 16 : 20

                property real morphP: {
                    var d = fullH - 40
                    return d > 0 ? Math.max(0, Math.min(1, (height - 40) / d)) : 1
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

                Connections {
                    target: root
                    function onHeadlinesChanged() { newsCard.newsIndex = 0 }
                }

                // opener — click the ticker to expand
                MouseArea {
                    id: newsOpen
                    anchors.fill: parent
                    enabled: !root.newsExpanded
                    hoverEnabled: true
                    cursorShape: Qt.PointingHandCursor
                    onClicked: root.newsExpanded = true
                }

                // ---- collapsed: ticker (pulsing dot + scrolling headline) ----
                Item {
                    id: tickerFace
                    anchors.left: parent.left
                    anchors.right: parent.right
                    anchors.top: parent.top
                    height: 40
                    opacity: 1 - Math.min(1, newsCard.morphP / 0.35)
                    visible: opacity > 0.01

                    Rectangle {
                        id: newsDot
                        anchors.left: parent.left
                        anchors.leftMargin: 17
                        anchors.verticalCenter: parent.verticalCenter
                        width: 7; height: 7; radius: 3.5
                        color: root.clockColor
                        SequentialAnimation on opacity {
                            running: newsCard.visible && !root.newsExpanded
                            loops: Animation.Infinite
                            NumberAnimation { to: 0.25; duration: 1100; easing.type: Easing.InOutSine }
                            NumberAnimation { to: 1.0; duration: 1100; easing.type: Easing.InOutSine }
                        }
                    }

                    Item {
                        id: tickerClip
                        anchors.left: newsDot.right
                        anchors.leftMargin: 12
                        anchors.right: parent.right
                        anchors.rightMargin: 16
                        anchors.verticalCenter: parent.verticalCenter
                        height: parent.height
                        clip: true

                        // skeleton line until headlines load
                        Rectangle {
                            anchors.left: parent.left
                            anchors.verticalCenter: parent.verticalCenter
                            width: parent.width * 0.8
                            height: 12
                            radius: 6
                            color: root.surface
                            visible: !newsCard.newsLoaded
                            SequentialAnimation on opacity {
                                running: !newsCard.newsLoaded
                                loops: Animation.Infinite
                                NumberAnimation { to: 0.35; duration: 700; easing.type: Easing.InOutSine }
                                NumberAnimation { to: 0.8; duration: 700; easing.type: Easing.InOutSine }
                            }
                        }

                        Text {
                            id: tickerText
                            anchors.verticalCenter: parent.verticalCenter
                            visible: newsCard.newsLoaded
                            text: root.headlines.length > 0 ? (root.headlines[newsCard.newsIndex].title || "") : ""
                            color: root.textColor
                            font.family: root.fontFamily
                            font.pixelSize: 12
                            property real slideY: 0
                            transform: Translate { y: tickerText.slideY }
                        }
                    }

                    SequentialAnimation {
                        id: newsCycle
                        running: newsCard.visible && !root.newsExpanded && root.headlines.length > 1
                        loops: Animation.Infinite
                        ScriptAction { script: { tickerText.x = 0; tickerText.slideY = 0 } }
                        PauseAnimation { duration: 1600 }
                        NumberAnimation {
                            target: tickerText; property: "x"
                            to: newsCard.scrollTo
                            duration: newsCard.scrollDur
                            easing.type: Easing.InOutSine
                        }
                        PauseAnimation { duration: newsCard.holdPad }
                        NumberAnimation { target: tickerText; property: "slideY"; to: -18; duration: 220; easing.type: Easing.InCubic }
                        ScriptAction {
                            script: {
                                if (root.headlines.length > 0)
                                    newsCard.newsIndex = (newsCard.newsIndex + 1) % root.headlines.length
                                tickerText.x = 0
                                tickerText.slideY = 18
                            }
                        }
                        NumberAnimation { target: tickerText; property: "slideY"; to: 0; duration: 220; easing.type: Easing.OutCubic }
                    }
                }

                // ---- expanded: headlines list ----
                Column {
                    anchors.fill: parent
                    anchors.margins: 14
                    spacing: 8
                    opacity: Math.max(0, (newsCard.morphP - 0.4) / 0.6)
                    visible: opacity > 0.01

                    // header (click to collapse)
                    Item {
                        width: parent.width
                        height: 22
                        Text {
                            anchors.left: parent.left
                            anchors.verticalCenter: parent.verticalCenter
                            text: "News"
                            color: root.subtext
                            font.family: root.fontFamily
                            font.pixelSize: 11
                        }
                        Text {
                            anchors.right: parent.right
                            anchors.verticalCenter: parent.verticalCenter
                            text: "NYT"
                            color: root.subtext
                            font.family: root.fontFamily
                            font.pixelSize: 9
                        }
                        MouseArea {
                            anchors.fill: parent
                            anchors.margins: -14
                            cursorShape: Qt.PointingHandCursor
                            onClicked: root.newsExpanded = false
                        }
                    }

                    Rectangle { width: parent.width; height: 1; color: root.surface }

                    ListView {
                        id: newsList
                        width: parent.width
                        height: newsCard.listH
                        clip: true
                        model: root.headlines
                        currentIndex: root.newsSelected
                        boundsBehavior: Flickable.StopAtBounds
                        spacing: 0

                        delegate: Rectangle {
                            required property var modelData
                            required property int index
                            property bool selected: index === root.newsSelected
                            width: ListView.view.width
                            height: 46
                            color: selected ? root.accent : hl.containsMouse ? root.surface : "transparent"
                            radius: 8

                            Behavior on color { ColorAnimation { duration: 100 } }

                            Column {
                                anchors.left: parent.left
                                anchors.leftMargin: 10
                                anchors.right: parent.right
                                anchors.rightMargin: 10
                                anchors.verticalCenter: parent.verticalCenter
                                spacing: 2

                                Text {
                                    width: parent.width
                                    text: modelData.title || ""
                                    color: parent.parent.selected ? root.bgColor : root.textColor
                                    font.family: root.fontFamily
                                    font.pixelSize: 12
                                    font.bold: parent.parent.selected
                                    elide: Text.ElideRight
                                }
                                Text {
                                    width: parent.width
                                    text: modelData.desc || ""
                                    color: parent.parent.selected ? root.bgColor : root.subtext
                                    font.family: root.fontFamily
                                    font.pixelSize: 10
                                    elide: Text.ElideRight
                                    visible: text.length > 0
                                }
                            }

                            MouseArea {
                                id: hl
                                anchors.fill: parent
                                hoverEnabled: true
                                cursorShape: Qt.PointingHandCursor
                                onEntered: root.newsSelected = index
                                onClicked: {
                                    if (modelData.link) { root.pendingUrl = modelData.link; openUrl.running = true }
                                    root.newsExpanded = false
                                }
                            }
                        }
                    }
                }

                // keyboard nav for the expanded news card (Ctrl+J/K, arrows, Enter, Esc)
                Item {
                    id: newsKeys
                    anchors.fill: parent
                    focus: root.newsExpanded
                    Keys.onPressed: event => {
                        if (!root.newsExpanded) return
                        if (event.key === Qt.Key_Escape) {
                            root.newsExpanded = false; event.accepted = true; return
                        }
                        if (event.key === Qt.Key_Return || event.key === Qt.Key_Enter) {
                            let h = root.headlines[root.newsSelected]
                            if (h && h.link) { root.pendingUrl = h.link; openUrl.running = true }
                            root.newsExpanded = false; event.accepted = true; return
                        }
                        let ctrl = (event.modifiers & Qt.ControlModifier)
                        let down = event.key === Qt.Key_Down || (ctrl && event.key === Qt.Key_J)
                        let up = event.key === Qt.Key_Up || (ctrl && event.key === Qt.Key_K)
                        if (down) {
                            if (root.newsSelected < root.headlines.length - 1) root.newsSelected++
                            event.accepted = true
                        } else if (up) {
                            if (root.newsSelected > 0) root.newsSelected--
                            event.accepted = true
                        }
                    }
                }

                Timer {
                    id: newsFocusTimer
                    interval: 50
                    property int tries: 0
                    onTriggered: {
                        newsKeys.forceActiveFocus()
                        tries++
                        if (!newsKeys.activeFocus && tries < 6) newsFocusTimer.restart()
                        else tries = 0
                    }
                }

                Connections {
                    target: root
                    function onNewsExpandedChanged() {
                        if (root.newsExpanded) {
                            root.newsSelected = 0
                            newsFocusTimer.start()
                        }
                    }
                    function onNewsSelectedChanged() {
                        if (root.newsExpanded) newsList.positionViewAtIndex(root.newsSelected, ListView.Contain)
                    }
                }
            }

            // == portfolio widget — morphs between pill and expanded card (one element) ==  // temporarily hidden
            /*
            Rectangle {
                id: portfolioCard
                visible: bar.isDp
                anchors.right: parent.right
                anchors.rightMargin: 17
                y: 6
                color: (!root.portfolioExpanded && pfOpen.containsMouse) ? root.surface : root.bgColor
                clip: true

                Behavior on color { ColorAnimation { duration: 120 } }

                property bool pfLoaded: root.portfolioValue > 0

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
                // hug the content once loaded; use the skeleton's width while loading
                property real collapsedW: pfLoaded ? (pfCollapsed.implicitWidth + 28) : 128
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

                // skeleton placeholder shown until the first value arrives
                Row {
                    anchors.right: parent.right
                    anchors.rightMargin: 15
                    anchors.top: parent.top
                    height: 40
                    spacing: 8
                    visible: !portfolioCard.pfLoaded && !root.portfolioExpanded

                    SequentialAnimation on opacity {
                        running: !portfolioCard.pfLoaded
                        loops: Animation.Infinite
                        NumberAnimation { to: 0.35; duration: 700; easing.type: Easing.InOutSine }
                        NumberAnimation { to: 0.8; duration: 700; easing.type: Easing.InOutSine }
                    }

                    Rectangle {
                        anchors.verticalCenter: parent.verticalCenter
                        width: 60; height: 12; radius: 6
                        color: root.surface
                    }
                    Rectangle {
                        anchors.verticalCenter: parent.verticalCenter
                        width: 34; height: 12; radius: 6
                        color: root.surface
                    }
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
                    visible: portfolioCard.pfLoaded && opacity > 0.01

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
            */
        }
    }

}
