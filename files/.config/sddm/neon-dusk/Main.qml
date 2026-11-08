import QtQuick
import QtQuick.Window
import QtQuick.Effects

Rectangle {
    id: root
    width: 1920
    height: 1080
    color: String(config.wallpaper).endsWith("startup-black.png") ? "#000000" : "#101a2b"

    // Xsetup selects the Samsung by its EDID, including when Xorg renames its output.
    readonly property bool loginScreen: primaryScreen
    readonly property string currentMonitor: Screen.name

    Image {
        id: wallpaper
        anchors.fill: parent
        source: Qt.resolvedUrl(config.wallpaper)
        fillMode: Image.PreserveAspectCrop
        sourceSize.width: root.width
        sourceSize.height: root.height
        visible: false
    }
    MultiEffect {
        anchors.fill: parent
        source: wallpaper
        blurEnabled: true
        blurMax: 48
        blur: Number(config.blur)
        autoPaddingEnabled: false
    }
    Rectangle { anchors.fill: parent; color: String(config.wallpaper).endsWith("startup-black.png") ? "transparent" : (root.loginScreen ? "#4010182b" : "#a310182b") }

    Loader {
        anchors.fill: parent
        source: root.loginScreen ? "LoginPanel.qml" : "NewsPanel.qml"
    }

    Component.onCompleted: Qt.callLater(function() {
        if (Window.window) {
            if (loginScreen)
                Window.window.requestActivate();
            else
                Window.window.flags |= Qt.WindowDoesNotAcceptFocus;
        }
        console.info("Neon Dusk:", currentMonitor,
            loginScreen ? "login" : "news", width + "x" + height);
    })
}
