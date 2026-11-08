import Quickshell
import Quickshell.Io
import QtQuick

Scope {
    id: palette
    property string mode: "dark"
    readonly property var definitions: JSON.parse(paletteFile.text() || "{}")
    readonly property var colors: definitions[mode] || definitions.dark || ({})
    readonly property color background: colors.background || "#08090b"
    readonly property color panel: colors.panel || "#ec111214"
    readonly property color card: colors.card || "#101113"
    readonly property color surface: colors.surface || "#242628"
    readonly property color paper: colors.paper || "#ebe9e2"
    readonly property color quiet: colors.quiet || "#969998"
    readonly property color muted: colors.muted || "#4b4f4c"
    readonly property color outline: colors.outline || "#343638"
    readonly property color accent: colors.accent || "#e6393f"
    readonly property color ink: colors.ink || "#101112"

    function select(value: string): void {
        if (["dark", "light", "black"].indexOf(value) < 0) return;
        mode = value;
        Quickshell.execDetached(["/usr/bin/python3", "/home/prayuj/.local/share/desktop-styles/nothing-orbit/apply-palette.py", value]);
    }
    FileView {
        id: paletteFile
        path: "/home/prayuj/.local/share/desktop-styles/nothing-orbit/palettes.json"
        blockLoading: true
    }
    FileView {
        id: preference
        path: "/home/prayuj/.local/state/desktop-styles/appearance.json"
        preload: true
        watchChanges: true
        onFileChanged: reload()
        onLoaded: {
            try {
                const value = JSON.parse(text()).mode;
                if (["dark", "light", "black"].indexOf(value) >= 0) palette.mode = value;
            } catch (e) {}
        }
    }
}
