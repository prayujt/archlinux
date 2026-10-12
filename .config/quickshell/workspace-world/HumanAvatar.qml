pragma ComponentBehavior: Bound
import QtQuick
import QtQuick3D
import "avatar"

Node {
    id: human
    required property real walkPhase
    required property real movementSpeed

    Model {
        source: "#Rectangle"
        y: 2.2
        eulerRotation.x: -90
        scale: Qt.vector3d(.8, .62, 1)
        materials: PrincipledMaterial {
            lighting: PrincipledMaterial.NoLighting
            alphaMode: PrincipledMaterial.Blend
            baseColorMap: Texture { source: "avatar/shadow.png" }
        }
    }

    Casual {
        scale: Qt.vector3d(100, 100, 100)
        eulerRotation.y: 180
        walkPhase: human.walkPhase
        movementSpeed: human.movementSpeed
    }
}
