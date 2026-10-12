pragma ComponentBehavior: Bound
import QtQuick
import QtQuick3D

Node {
    id: scenery
    required property var landmark
    required property var facadeMaterial
    position: Qt.vector3d(landmark.x, landmark.y, landmark.z)
    readonly property bool building: landmark.kind === "building"
    PrincipledMaterial { id: trim; baseColor: "#52585f"; roughness: 0.95 }
    PrincipledMaterial { id: dark; baseColor: "#292d32"; roughness: 0.9 }
    PrincipledMaterial { id: wood; baseColor: "#52473d"; roughness: 1 }
    PrincipledMaterial { id: foliage; baseColor: "#304d3c"; roughness: 1 }
    PrincipledMaterial { id: lamp; lighting: PrincipledMaterial.NoLighting; baseColor: "#ecd6a5" }
    PrincipledMaterial {
        id: facade
        roughness: scenery.landmark.style === 3 ? 0.5 : 0.9
        baseColorMap: Texture {
            source: "city/facade-" + (scenery.landmark.style || 0) + ".png"
            scaleU: (scenery.landmark.w || 800) / 800
            scaleV: (scenery.landmark.h || 2560) / 2560
            tilingModeHorizontal: Texture.Repeat
            tilingModeVertical: Texture.Repeat
            generateMipmaps: true
            mipFilter: Texture.Linear
        }
    }
    PrincipledMaterial { id: storefront; baseColor: "#334b62"; metalness: 0.15; roughness: 0.4 }

    Node {
        visible: scenery.building
        Model {
            source: "#Cube"
            pickable: true
            readonly property bool webSurface: true
            y: (scenery.landmark.h || 0) / 2
            scale: Qt.vector3d((scenery.landmark.w || 1) / 100, (scenery.landmark.h || 1) / 100, (scenery.landmark.d || 1) / 100)
            materials: facade
        }
        Model {
            source: "#Cube"
            y: scenery.landmark.h || 0
            scale: Qt.vector3d((scenery.landmark.w || 1) / 100 + .1, .10, (scenery.landmark.d || 1) / 100 + .1)
            materials: trim
        }
        Node {
            visible: !scenery.landmark.skyline
            Model {
                source: "#Cube"
                y: 14
                scale: Qt.vector3d((scenery.landmark.w || 1) / 100 + 0.14, 0.28, (scenery.landmark.d || 1) / 100 + 0.14)
                materials: trim
            }
            Model {
                source: "#Cube"
                position: Qt.vector3d(0, 85, (scenery.landmark.d || 0) / 2 + 1)
                scale: Qt.vector3d((scenery.landmark.w || 1) / 100 - 0.3, 1.35, 0.04)
                materials: storefront
            }
            Model {
                source: "#Cube"
                position: Qt.vector3d(0, 95, (scenery.landmark.d || 0) / 2 + 4)
                scale: Qt.vector3d(0.75, 1.9, 0.04)
                materials: dark
            }
            Model {
                source: "#Cube"
                position: Qt.vector3d(0, 177, (scenery.landmark.d || 0) / 2 + 22)
                scale: Qt.vector3d((scenery.landmark.w || 1) / 150, 0.08, 0.46)
                materials: dark
            }
        }
        Node {
            visible: !!scenery.landmark.spire
            y: scenery.landmark.h || 0
            Model { source: "#Cube"; y: 110; scale: Qt.vector3d(2.8, 2.2, 2.8); materials: scenery.facadeMaterial }
            Model { source: "#Cube"; y: 295; scale: Qt.vector3d(1.7, 1.5, 1.7); materials: scenery.facadeMaterial }
            Model { source: "#Cone"; y: 455; scale: Qt.vector3d(.65, 2, .65); materials: trim }
        }
        Node {
            visible: !!scenery.landmark.tank
            y: scenery.landmark.h || 0
            Model { source: "#Cube"; y: 30; scale: Qt.vector3d(.7, .6, .7); materials: dark }
            Model { source: "#Cylinder"; y: 105; scale: Qt.vector3d(1.1, 1.1, 1.1); materials: wood }
            Model { source: "#Cone"; y: 175; scale: Qt.vector3d(1.25, .45, 1.25); materials: dark }
        }
    }
    Node {
        visible: scenery.landmark.kind === "lamp"
        Model { source: "#Cylinder"; y: 135; scale: Qt.vector3d(.065, 2.7, .065); materials: dark }
        Model { source: "#Cube"; position: Qt.vector3d(-23, 266, 0); scale: Qt.vector3d(.55, .07, .2); materials: lamp }
    }
    Node {
        visible: scenery.landmark.kind === "tree"
        scale: Qt.vector3d(scenery.landmark.scale || 1, scenery.landmark.scale || 1, scenery.landmark.scale || 1)
        Model { source: "#Cylinder"; y: 80; scale: Qt.vector3d(.17, 1.6, .17); materials: wood }
        Model { source: "#Sphere"; y: 210; scale: Qt.vector3d(1.6, 1.9, 1.4); materials: foliage }
    }
    Node {
        visible: scenery.landmark.kind === "car"
        PrincipledMaterial { id: paint; baseColor: scenery.landmark.color || "#d3ad42"; roughness: .45 }
        Model { source: "#Cube"; y: 30; scale: Qt.vector3d(1.9, .45, .9); materials: paint }
        Model { source: "#Cube"; position: Qt.vector3d(0, 65, 0); scale: Qt.vector3d(1, .35, .75); materials: dark }
        Model { source: "#Cube"; position: Qt.vector3d(0, 10, 0); scale: Qt.vector3d(1.6, .25, 1); materials: dark }
    }
}
