pragma ComponentBehavior: Bound
import QtQuick
import QtQuick3D

Node {
    id: hands
    required property real walkPhase
    required property real movementSpeed
    required property bool airborne
    required property bool webActive
    required property real turnSway
    required property vector3d webDirection
    readonly property vector3d webHand: webEmitter.scenePosition
    readonly property real bob: airborne ? 0 : Math.sin(walkPhase) * Math.min(1.2, movementSpeed / 300)
    property real pose: webActive ? 1 : 0
    property real kick: 0
    readonly property vector3d leftWrist: Qt.vector3d(-26 - turnSway * 0.35, -35 + bob + pose * 5, -56)
    readonly property vector3d rightWrist: Qt.vector3d(26 - pose * 8 - turnSway * 0.5,
                                                      -32 - bob + pose * 14 + kick * 3,
                                                      -58 - pose * 12 - kick * 5)
    Behavior on pose { NumberAnimation { duration: 180; easing.type: Easing.OutCubic } }
    function shoot() { flick.restart(); }
    function normalized(q) {
        const length = Math.hypot(q.scalar, q.x, q.y, q.z);
        return length < 0.0001 ? Qt.quaternion(0, 1, 0, 0)
                              : Qt.quaternion(q.scalar / length, q.x / length, q.y / length, q.z / length);
    }
    function aimRotation() {
        const d = webDirection.normalized();
        const q = normalized(Qt.quaternion(1 + d.y, d.z, 0, -d.x));
        const idle = normalized(Qt.quaternion(0.86, -0.51, 0.05, 0.04));
        const sign = q.scalar * idle.scalar + q.x * idle.x + q.y * idle.y + q.z * idle.z < 0 ? -1 : 1;
        return normalized(Qt.quaternion(idle.scalar * (1 - pose) + q.scalar * pose * sign,
                             idle.x * (1 - pose) + q.x * pose * sign,
                             idle.y * (1 - pose) + q.y * pose * sign,
                             idle.z * (1 - pose) + q.z * pose * sign));
    }
    function directionRotation(direction) {
        const d = direction.normalized();
        return normalized(Qt.quaternion(1 + d.y, d.z, 0, -d.x));
    }

    SequentialAnimation {
        id: flick
        NumberAnimation { target: hands; property: "kick"; from: 0; to: 1; duration: 55; easing.type: Easing.OutCubic }
        NumberAnimation { target: hands; property: "kick"; to: 0; duration: 170; easing.type: Easing.OutCubic }
    }
    PrincipledMaterial { id: fabric; baseColor: "#173552"; roughness: 0.75 }
    PrincipledMaterial {
        id: glove
        roughness: 0.65
        baseColorMap: Texture { source: "avatar/hands/glove.png"; generateMipmaps: true; mipFilter: Texture.Linear }
    }
    PrincipledMaterial { id: cuff; baseColor: "#9a2938"; roughness: 0.7 }
    MorphTarget { id: fingerGesture; attributes: MorphTarget.Position | MorphTarget.Normal; weight: hands.pose }
    Model {
        readonly property vector3d direction: hands.leftWrist.minus(Qt.vector3d(-32, -62, -20))
        readonly property real stretch: direction.length() / 23.5
        source: "avatar/hands/meshes/forearm_L_mesh.mesh"
        position: hands.leftWrist.minus(direction.normalized().times(3 * stretch - 2))
        scale: Qt.vector3d(0.75, stretch, 0.75)
        rotation: hands.directionRotation(direction)
        materials: fabric
    }
    Model {
        readonly property vector3d direction: hands.rightWrist.minus(Qt.vector3d(32, -62, -20))
        readonly property real stretch: direction.length() / 23.5
        source: "avatar/hands/meshes/forearm_R_mesh.mesh"
        position: hands.rightWrist.minus(direction.normalized().times(3 * stretch - 2))
        scale: Qt.vector3d(0.75, stretch, 0.75)
        rotation: hands.directionRotation(direction)
        materials: fabric
    }
    Model {
        id: leftHand
        source: "avatar/hands/meshes/hand_L_mesh.mesh"
        scale: Qt.vector3d(0.75, 0.75, 0.75)
        position: hands.leftWrist
        eulerRotation: Qt.vector3d(-48 - hands.pose * 8, -12, -12)
        materials: glove
    }
    Model {
        id: rightHand
        source: "avatar/hands/meshes/hand_R_mesh.mesh"
        scale: Qt.vector3d(0.75, 0.75, 0.75)
        position: hands.rightWrist
        rotation: hands.aimRotation()
        materials: glove
        morphTargets: [fingerGesture]
        Node {
            id: webEmitter
            position: Qt.vector3d(0, 6, -2.3)
        }
    }
    Model {
        source: "#Cylinder"
        position: leftHand.mapPositionToScene(Qt.vector3d(0, 1, 0))
        rotation: leftHand.rotation
        scale: Qt.vector3d(0.065, 0.05, 0.052)
        materials: cuff
    }
    Model {
        source: "#Cylinder"
        position: rightHand.mapPositionToScene(Qt.vector3d(0, 1, 0))
        rotation: rightHand.rotation
        scale: Qt.vector3d(0.065, 0.05, 0.052)
        materials: cuff
    }
}
