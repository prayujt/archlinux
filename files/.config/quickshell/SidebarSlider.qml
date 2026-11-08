import QtQuick
import QtQuick.Controls.Basic as Controls

Controls.Slider {
    id: levelControl
    required property var colors
    hoverEnabled: true
    from: 0; to: 100; stepSize: 1
    padding: 6
    implicitHeight: 28
    background: Rectangle {
        x: levelControl.leftPadding
        y: levelControl.topPadding + (levelControl.availableHeight - height) / 2
        width: levelControl.availableWidth; height: 4; radius: 2
        color: levelControl.colors.surface
        Rectangle {
            width: levelControl.visualPosition * parent.width; height: parent.height; radius: parent.radius
            color: levelControl.colors.paper
            Behavior on width { enabled: !levelControl.pressed; NumberAnimation { duration: 120; easing.type: Easing.OutCubic } }
        }
    }
    handle: Rectangle {
        x: levelControl.leftPadding + levelControl.visualPosition * (levelControl.availableWidth - width)
        y: levelControl.topPadding + (levelControl.availableHeight - height) / 2
        width: 12; height: 12; radius: 6
        color: levelControl.colors.paper
        opacity: levelControl.hovered || levelControl.pressed ? 1 : 0
        Behavior on opacity { NumberAnimation { duration: 90 } }
        Behavior on x { enabled: !levelControl.pressed; NumberAnimation { duration: 120; easing.type: Easing.OutCubic } }
    }
}
