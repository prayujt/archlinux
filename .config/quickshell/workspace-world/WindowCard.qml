pragma ComponentBehavior: Bound
import Quickshell.Wayland
import QtQuick
import QtQuick3D

Node {
    id: card
    required property var windowHandle
    required property int workspaceId
    required property color accent
    required property bool livePreview
    required property var hovered
    required property real cardWidth
    required property var worldView
    readonly property real cardHeight: Math.min(310, cardWidth * 0.72)
    readonly property bool highlighted: hovered === face
    readonly property bool hasPreview: preview.hasContent
    opacity: worldView.panelOpacity(card, cardWidth, cardHeight)
    Behavior on opacity { NumberAnimation { duration: 140 } }

    Model {
        source: "#Cube"
        scale: Qt.vector3d((card.cardWidth + 3) / 100, (card.cardHeight + 3) / 100, 0.045)
        z: -4
        materials: PrincipledMaterial {
            lighting: PrincipledMaterial.NoLighting
            baseColor: card.highlighted ? card.accent : "#596c79"
            emissiveFactor: Qt.vector3d(1.0, 1.0, 1.0)
        }
    }
    Model {
        id: face
        source: "#Rectangle"
        scale: Qt.vector3d(card.cardWidth / 100, card.cardHeight / 100, 1)
        pickable: true
        readonly property var windowHandle: card.windowHandle
        readonly property int workspaceId: card.workspaceId
        materials: PrincipledMaterial {
            lighting: PrincipledMaterial.NoLighting
            cullMode: Material.NoCulling
            baseColorMap: Texture {
                sourceItem: Rectangle {
                    id: textureFrame
                    width: card.livePreview ? 1280 : 640
                    height: width * card.cardHeight / card.cardWidth
                    color: "#0b1625"
                    Rectangle {
                        width: parent.width; height: textureFrame.width / 15
                        gradient: Gradient {
                            orientation: Gradient.Horizontal
                            GradientStop { position: 0; color: "#214258" }
                            GradientStop { position: 1; color: "#172338" }
                        }
                        Rectangle { x: parent.height * .3; y: parent.height * .38; width: parent.height * .2; height: width; radius: width / 2; color: card.accent }
                        Text {
                            x: parent.height * .8; anchors.verticalCenter: parent.verticalCenter
                            width: parent.width - x - 20; elide: Text.ElideRight
                            text: card.windowHandle?.title || "Window"
                            color: "#e7f2ff"
                            font { family: "GeistMono Nerd Font Mono"; pixelSize: textureFrame.width / 42 }
                        }
                    }
                    ScreencopyView {
                        id: preview
                        x: 0; y: textureFrame.width / 15; width: parent.width; height: parent.height - y
                        captureSource: card.windowHandle?.wayland || null
                        constraintSize: Qt.size(textureFrame.width, textureFrame.width * .8)
                        live: false
                        paintCursor: false
                        Timer { interval: 500; running: !card.livePreview; onTriggered: if (!preview.hasContent) preview.captureFrame() }
                        Timer { interval: 100; running: card.livePreview; repeat: true; onTriggered: preview.captureFrame() }
                    }
                    Text {
                        anchors.centerIn: preview
                        visible: !preview.hasContent
                        width: parent.width - 80
                        text: card.windowHandle?.wayland?.appId || card.windowHandle?.lastIpcObject?.class || "Window preview"
                        horizontalAlignment: Text.AlignHCenter
                        wrapMode: Text.Wrap
                        color: card.accent
                        font { family: "GeistMono Nerd Font Mono"; pixelSize: 28; bold: true }
                    }
                }
            }
        }
    }
}
