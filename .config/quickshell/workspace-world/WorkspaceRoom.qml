pragma ComponentBehavior: Bound
import Quickshell.Hyprland
import QtQuick
import QtQuick3D

Node {
    id: room
    required property int workspaceId
    required property color accent
    required property bool selected
    required property var hovered
    required property var worldView
    readonly property var workspace: Hyprland.workspaces.values.find(w => w.id === workspaceId) || null
    readonly property var windows: workspace ? workspace.toplevels.values : []
    readonly property int windowCount: windows.length
    function previewCount() {
        let count = 0;
        for (let i = 0; i < cards.count; i++) {
            const card = cards.objectAt(i) as WindowCard;
            if (card?.hasPreview) count++;
        }
        return count;
    }

    PrincipledMaterial { id: structure; baseColor: "#41474e"; roughness: .95 }
    PrincipledMaterial { id: pavement; baseColor: "#6a7077"; roughness: 1 }
    Model {
        source: "#Cube"
        scale: Qt.vector3d(10, .05, 8)
        y: 1
        materials: pavement
    }
    Model {
        source: "#Cube"
        position: Qt.vector3d(-420, 55, 170)
        scale: Qt.vector3d(.05, 1.1, .05)
        materials: structure
    }
    Model {
        id: sign
        source: "#Rectangle"
        position: Qt.vector3d(-420, 115, 172)
        scale: Qt.vector3d(1.15, .35, 1)
        pickable: true
        opacity: room.worldView.panelOpacity(sign, 115, 35)
        Behavior on opacity { NumberAnimation { duration: 140 } }
        readonly property int workspaceId: room.workspaceId
        materials: PrincipledMaterial {
            lighting: PrincipledMaterial.NoLighting
            cullMode: Material.NoCulling
            baseColorMap: Texture {
                sourceItem: Rectangle {
                    width: 345; height: 105; radius: 4
                    color: "#242b32"
                    Text {
                        anchors.centerIn: parent
                        text: "WS " + String(room.workspaceId).padStart(2, "0")
                        color: room.selected ? "#c8dce1" : "#9aa4ae"
                        font { family: "GeistMono Nerd Font Mono"; pixelSize: 38; letterSpacing: 2 }
                    }
                }
            }
        }
    }

    Repeater3D {
        id: cards
        model: room.windows
        delegate: WindowCard {
            required property int index
            required property var modelData
            windowHandle: modelData
            workspaceId: room.workspaceId
            accent: room.accent
            livePreview: room.selected && room.visible
            hovered: room.hovered
            worldView: room.worldView
            x: room.windowCount === 1 ? 0 : ((index % 3) - (Math.min(room.windowCount, 3) - 1) / 2) * 330
            y: 215 + Math.floor(index / 3) * 335
            z: -130 - Math.floor(index / 3) * 90
            eulerRotation.y: room.windowCount === 1 ? 0 : -x / 35
            cardWidth: room.windowCount === 1 ? 520 : 305
        }
    }

}
