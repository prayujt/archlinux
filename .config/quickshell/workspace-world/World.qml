pragma ComponentBehavior: Bound
import QtQuick
import QtQuick3D
import QtQuick3D.Helpers
import "WorldMap.js" as Map
import "WorldPhysics.js" as Physics

Item {
    id: world
    required property int initialWorkspace
    signal closeRequested()
    signal workspaceSelected(int id, var toplevel)
    property real yaw: 0
    property real pitch: -6
    property real targetYaw: 0
    property real targetPitch: -6
    property vector3d avatarPosition: Qt.vector3d(0, 0, 0)
    property real walkPhase: 0
    property vector2d velocity: Qt.vector2d(0, 0)
    property var motion: Physics.initial(0, 42, 0)
    property bool mouseWeb: false
    property bool showControls: true
    property string feedback: ""
    property bool overview: false
    property var heldKeys: ({})
    property var hovered: null
    property real walkYaw: 0
    property real walkPitch: -6
    property vector3d cameraAnchor: Qt.vector3d(0, 0, 0)
    property real swingRoll: 0
    readonly property real cameraAimHeight: 165
    readonly property int currentWorkspace: nearestWorkspace()
    readonly property real workspaceDistance: Math.hypot(avatarPosition.x - Map.sectors[currentWorkspace - 1].x, avatarPosition.z - Map.sectors[currentWorkspace - 1].z)
    readonly property vector3d followPosition: cameraFollowPosition()
    readonly property real cameraGap: Math.hypot(cameraAnchor.x - avatarPosition.x, cameraAnchor.y - avatarPosition.y, cameraAnchor.z - avatarPosition.z)
    readonly property bool edgeLookEnabled: inputArea.containsMouse && !lookDrag.active && !overview
    readonly property real edgeX: edgeLookEnabled ? edgeStrength(pointer.x, width) : 0
    readonly property real edgeY: edgeLookEnabled ? edgeStrength(pointer.y, height) : 0

    function edgeStrength(position, size) {
        const band = 85;
        const t = position < band ? -(1 - position / band)
                                  : (position > size - band ? 1 - (size - position) / band : 0);
        const amount = Math.min(1, Math.abs(t));
        return Math.sign(t) * amount * amount * (3 - 2 * amount);
    }

    function roomPosition(id) {
        const sector = Map.sectors[id - 1];
        return Qt.vector3d(sector.x, sector.y, sector.z);
    }
    function nearestWorkspace() {
        let id = 1, nearest = Infinity;
        for (const sector of Map.sectors) {
            const distance = Math.hypot(avatarPosition.x - sector.x, avatarPosition.z - sector.z);
            if (distance < nearest) { id = sector.id; nearest = distance; }
        }
        return id;
    }
    function cameraFollowPosition() {
        return Qt.vector3d(avatarPosition.x, Math.max(cameraAnchor.y + cameraAimHeight, avatarPosition.y + 100), avatarPosition.z);
    }
    function panelOpacity(panel, panelWidth, panelHeight) {
        if (overview) return 1;
        const eye = panel.mapPositionFromScene(walkCamera.scenePosition);
        return Math.abs(eye.z) < 100 && Math.abs(eye.x) < panelWidth / 2 + 55 && Math.abs(eye.y) < panelHeight / 2 + 50 ? 0.08 : 1;
    }
    function roomColor(id) {
        return ["#4ee6e8", "#7b9eff", "#b0e86f", "#f3c969", "#4ec6ff"][(id - 1) % 5];
    }
    function updateMotion(next) {
        motion = next;
        avatarPosition = Qt.vector3d(next.x, next.y, next.z);
        velocity = Qt.vector2d(next.vx, next.vz);
    }
    function fireWeb(aimed = false) {
        if (overview || trip.running) return;
        let point = null;
        if (aimed && inputArea.containsMouse) {
            const hit = view.pickAll(pointer.x, pointer.y).find(p => p.objectHit?.webSurface);
            if (hit && hit.scenePosition.minus(avatarPosition).length() < 3900)
                point = [hit.scenePosition.x, hit.scenePosition.y, hit.scenePosition.z];
        }
        if (!point) point = Physics.webTarget(motion, yaw);
        const next = Physics.shoot(motion, point, yaw);
        hands.shoot();
        if (!next.anchor) { feedback = "Aim toward a nearby building"; feedbackTimer.restart(); }
        updateMotion(next);
    }
    function releaseWeb() { updateMotion(Physics.release(motion)); }
    function travel(position, newYaw, newPitch, animated) {
        trip.stop();
        heldKeys = {};
        velocity = Qt.vector2d(0, 0);
        mouseWeb = false;
        targetYaw = newYaw;
        targetPitch = newPitch;
        if (animated) {
            travelPosition.from = walkCamera.position;
            travelPosition.to = position;
            travelYaw.from = yaw;
            travelYaw.to = newYaw;
            travelPitch.from = pitch;
            travelPitch.to = newPitch;
            trip.start();
        } else {
            walkCamera.position = position;
            yaw = newYaw;
            pitch = newPitch;
        }
        forceActiveFocus();
    }
    function visit(id, animated = true) {
        overview = false;
        const p = roomPosition(id);
        const rotation = Map.sectors[id - 1].rotation;
        const angle = rotation * Math.PI / 180;
        avatarPosition = Qt.vector3d(p.x + Math.sin(angle) * 390, p.y + 2, p.z + Math.cos(angle) * 390);
        updateMotion(Physics.initial(avatarPosition.x, avatarPosition.y, avatarPosition.z));
        cameraAnchor = avatarPosition;
        yaw = rotation;
        pitch = -6;
        swingRoll = 0;
        travel(cameraFollowPosition(), rotation, -6, animated);
    }
    function toggleOverview() {
        heldKeys = {};
        releaseWeb();
        if (!overview) {
            walkYaw = yaw;
            walkPitch = pitch;
            overview = true;
            travel(Qt.vector3d(0, 8500, 6200), 0, -54, true);
        } else {
            overview = false;
            yaw = walkYaw;
            pitch = walkPitch;
            travel(cameraFollowPosition(), walkYaw, walkPitch, true);
        }
    }
    function pickAt(x, y) {
        const hit = view.pick(x, y).objectHit;
        if (hit?.windowHandle) workspaceSelected(hit.workspaceId, hit.windowHandle);
        else if (hit?.workspaceId) workspaceSelected(hit.workspaceId, null);
    }
    function status() {
        let cards = 0, previews = 0;
        for (let i = 0; i < rooms.count; i++) {
            const room = rooms.objectAt(i) as WorkspaceRoom;
            if (room) {
                cards += room.windowCount;
                previews += room.previewCount();
            }
        }
        const webScreen = view.mapFrom3DScene(web.start);
        const handScreen = handsView.mapFrom3DScene(hands.webHand);
        return { workspace: currentWorkspace, overview: overview,
                 camera: [walkCamera.x, walkCamera.y, walkCamera.z], windows: cards, previews: previews,
                 fps: view.renderStats.fps, renderMs: view.renderStats.renderTime,
                 facing: [yaw, pitch], velocity: [velocity.x, velocity.y],
                 pointer: [pointer.x, pointer.y], hover: inputArea.containsMouse,
                 focus: activeFocus, edge: [edgeX, edgeY],
                 avatar: [avatarPosition.x, avatarPosition.y, avatarPosition.z], viewMode: "firstPerson", seed: Map.seed,
                 grounded: motion.grounded, verticalSpeed: motion.vy, webAnchor: motion.anchor,
                 ropeLength: motion.ropeLength, webShot: motion.shot,
                 webOrigin: [web.start.x, web.start.y, web.start.z],
                 webAlignmentError: Math.hypot(webScreen.x - handScreen.x, webScreen.y - handScreen.y) };
    }

    focus: true
    Component.onCompleted: visit(initialWorkspace, false)
    Timer { interval: 250; running: true; onTriggered: world.forceActiveFocus() }
    Timer { id: controlsTimer; interval: 10000; running: true; onTriggered: world.showControls = false }
    Timer { id: feedbackTimer; interval: 1300; onTriggered: world.feedback = "" }
    onActiveFocusChanged: if (!activeFocus) { heldKeys = {}; mouseWeb = false; releaseWeb(); }
    Keys.onPressed: event => {
        if (event.key === Qt.Key_Escape) closeRequested();
        else if (event.key === Qt.Key_Tab && !event.isAutoRepeat) toggleOverview();
        else if (event.key === Qt.Key_H && !event.isAutoRepeat) { controlsTimer.stop(); showControls = !showControls; }
        else if (event.key === Qt.Key_R && !event.isAutoRepeat) visit(currentWorkspace);
        else if (event.key === Qt.Key_Space && !event.isAutoRepeat && !overview && !trip.running)
            updateMotion(Physics.jump(motion));
        else if (event.key === Qt.Key_Return || event.key === Qt.Key_Enter) {
            if (workspaceDistance < 850) workspaceSelected(currentWorkspace, null);
        }
        else if ([Qt.Key_W, Qt.Key_A, Qt.Key_S, Qt.Key_D, Qt.Key_Up, Qt.Key_Down,
                  Qt.Key_Left, Qt.Key_Right, Qt.Key_Shift, Qt.Key_Q, Qt.Key_E, Qt.Key_F].includes(event.key)) {
            if (event.key === Qt.Key_F && !event.isAutoRepeat) fireWeb();
            const keys = Object.assign({}, heldKeys);
            keys[event.key] = true;
            heldKeys = keys;
        } else { event.accepted = false; return; }
        event.accepted = true;
    }
    Keys.onReleased: event => {
        if (event.isAutoRepeat) return;
        const keys = Object.assign({}, heldKeys);
        delete keys[event.key];
        heldKeys = keys;
        if (event.key === Qt.Key_F && !mouseWeb) releaseWeb();
    }

    ParallelAnimation {
        id: trip
        Vector3dAnimation { id: travelPosition; target: walkCamera; property: "position"; duration: 650; easing.type: Easing.InOutCubic }
        NumberAnimation { id: travelYaw; target: world; property: "yaw"; duration: 650; easing.type: Easing.InOutCubic }
        NumberAnimation { id: travelPitch; target: world; property: "pitch"; duration: 650; easing.type: Easing.InOutCubic }
    }
    FrameAnimation {
        running: (world.activeFocus || inputArea.containsMouse) && !trip.running
                 && ((!world.overview && (Object.keys(world.heldKeys).length > 0 || world.velocity.length() > 0.5 || world.cameraGap > 0.2
                                         || !world.motion.grounded || world.motion.anchor || Math.abs(world.swingRoll) > 0.05))
                     || world.edgeX !== 0 || world.edgeY !== 0
                     || Math.abs(world.yaw - world.targetYaw) > 0.01 || Math.abs(world.pitch - world.targetPitch) > 0.01)
        onTriggered: {
            const k = world.heldKeys;
            const dt = Math.min(frameTime, 0.05);
            const forward = (k[Qt.Key_W] || k[Qt.Key_Up] ? 1 : 0) - (k[Qt.Key_S] || k[Qt.Key_Down] ? 1 : 0);
            const right = (k[Qt.Key_D] || k[Qt.Key_Right] ? 1 : 0) - (k[Qt.Key_A] || k[Qt.Key_Left] ? 1 : 0);
            world.targetYaw += ((k[Qt.Key_Q] ? 1 : 0) - (k[Qt.Key_E] ? 1 : 0)) * 90 * dt;
            world.targetYaw -= world.edgeX * 95 * dt;
            world.targetPitch = Math.max(-75, Math.min(70, world.targetPitch - world.edgeY * 40 * dt));
            const turnBlend = 1 - Math.exp(-22 * dt);
            world.yaw += (world.targetYaw - world.yaw) * turnBlend;
            world.pitch += (world.targetPitch - world.pitch) * turnBlend;
            if (world.overview) return;
            const angle = world.yaw * Math.PI / 180;
            const magnitude = Math.max(1, Math.hypot(forward, right));
            world.updateMotion(Physics.step(world.motion, {
                x: (right * Math.cos(angle) - forward * Math.sin(angle)) / magnitude,
                z: -(forward * Math.cos(angle) + right * Math.sin(angle)) / magnitude,
                run: !!k[Qt.Key_Shift]
            }, dt));
            const moving = world.velocity.length();
            world.walkPhase += moving * dt / (27 + Math.max(0, moving - 200) * 0.045);
            const followBlend = 1 - Math.exp(-18 * dt);
            world.cameraAnchor = Qt.vector3d(world.avatarPosition.x,
                                            world.cameraAnchor.y + (world.avatarPosition.y - world.cameraAnchor.y) * followBlend,
                                            world.avatarPosition.z);
            const sideSpeed = world.velocity.x * Math.cos(angle) - world.velocity.y * Math.sin(angle);
            const roll = world.motion.anchor ? Math.max(-5, Math.min(5, -sideSpeed / 300)) : 0;
            world.swingRoll += (roll - world.swingRoll) * (1 - Math.exp(-6 * dt));
            walkCamera.position = world.followPosition;
        }
    }

    View3D {
        id: view
        anchors.fill: parent
        camera: walkCamera
        explicitTextureWidth: Math.min(width, 2560)
        explicitTextureHeight: Math.round(height * Math.min(1, 2560 / width))
        environment: SceneEnvironment {
            backgroundMode: SceneEnvironment.SkyBox
            lightProbe: Texture {
                textureData: ProceduralSkyTextureData {
                    skyTopColor: "#192b43"
                    skyHorizonColor: "#9a8271"
                    groundBottomColor: "#18212c"
                    groundHorizonColor: "#9a8271"
                    sunColor: "#e9c290"
                    sunLatitude: 8
                    sunLongitude: -75
                    skyEnergy: .45
                    groundEnergy: .3
                    sunEnergy: .7
                }
            }
            antialiasingMode: SceneEnvironment.MSAA
            antialiasingQuality: SceneEnvironment.Medium
            tonemapMode: SceneEnvironment.TonemapModeLinear
            fog: Fog { enabled: true; depthEnabled: true; depthNear: 3000; depthFar: 17000; density: 0.4; color: "#65707d" }
        }
        PerspectiveCamera {
            id: walkCamera
            clipNear: 5
            clipFar: 22000
            fieldOfView: 76 + (world.overview ? 0 : Math.min(5, world.velocity.length() / 360))
            Behavior on fieldOfView { NumberAnimation { duration: 220 } }
            eulerRotation: Qt.vector3d(world.pitch, world.yaw, world.overview ? 0 : world.swingRoll)
            Node {
                id: webOrigin
                position: hands.webHand
            }
        }
        DirectionalLight {
            eulerRotation: Qt.vector3d(-35, -55, 0)
            color: "#f1dcc3"
            brightness: 1.1
            ambientColor: "#9daabb"
            castsShadow: true
            shadowMapQuality: Light.ShadowMapQualityHigh
            shadowMapFar: 6500
            shadowFactor: 55
            csmNumSplits: 1
        }

        Node {
            id: cityMaterials
            readonly property var facades: [brick0, brick1, brick2, brick3, brick4, brick5]
            PrincipledMaterial { id: brick0; roughness: .9; baseColorMap: Texture { source: "city/facade-0.png"; generateMipmaps: true; mipFilter: Texture.Linear } }
            PrincipledMaterial { id: brick1; roughness: .9; baseColorMap: Texture { source: "city/facade-1.png"; generateMipmaps: true; mipFilter: Texture.Linear } }
            PrincipledMaterial { id: brick2; roughness: .85; baseColorMap: Texture { source: "city/facade-2.png"; generateMipmaps: true; mipFilter: Texture.Linear } }
            PrincipledMaterial { id: brick3; roughness: .55; baseColorMap: Texture { source: "city/facade-3.png"; generateMipmaps: true; mipFilter: Texture.Linear } }
            PrincipledMaterial { id: brick4; roughness: .9; baseColorMap: Texture { source: "city/facade-4.png"; generateMipmaps: true; mipFilter: Texture.Linear } }
            PrincipledMaterial { id: brick5; roughness: .65; baseColorMap: Texture { source: "city/facade-5.png"; generateMipmaps: true; mipFilter: Texture.Linear } }
        }

        Model {
            source: "#Rectangle"
            y: 39
            eulerRotation.x: -90
            scale: Qt.vector3d(240, 220, 1)
            materials: PrincipledMaterial { baseColor: "#303944"; roughness: 1 }
        }
        Model {
            y: Map.elevation / 2
            geometry: HeightFieldGeometry {
                source: "heightmap.png"
                extents: Qt.vector3d(Map.width, Map.elevation, Map.depth)
                smoothShading: true
            }
            materials: PrincipledMaterial {
                roughness: 1
                baseColorMap: Texture { source: "terrain-color.png"; generateMipmaps: true; mipFilter: Texture.Linear }
            }
        }
        Repeater3D {
            id: rooms
            model: 20
            delegate: WorkspaceRoom {
                required property int index
                workspaceId: index + 1
                position: world.roomPosition(workspaceId)
                eulerRotation.y: Map.sectors[index].rotation
                accent: world.roomColor(workspaceId)
                selected: !world.overview && world.currentWorkspace === workspaceId && world.workspaceDistance < 850
                visible: world.overview || Math.hypot(world.avatarPosition.x - x, world.avatarPosition.z - z) < 4500
                hovered: world.hovered
                worldView: world
            }
        }
        Repeater3D {
            model: Map.props
            delegate: Scenery {
                required property var modelData
                landmark: modelData
                facadeMaterial: cityMaterials.facades[modelData.style || 0]
                visible: modelData.skyline || world.overview || Math.hypot(world.avatarPosition.x - x, world.avatarPosition.z - z) < 5500
            }
        }
        Node {
            id: web
            visible: !world.overview && !!world.motion.anchor
            readonly property vector3d start: webOrigin.scenePosition
            readonly property vector3d anchor: world.motion.anchor
                ? Qt.vector3d(world.motion.anchor[0], world.motion.anchor[1], world.motion.anchor[2]) : start
            readonly property vector3d end: start.plus(anchor.minus(start).times(world.motion.shot))
            readonly property vector3d delta: end.minus(start)
            readonly property real sag: Math.min(140, Math.max(0, world.motion.ropeLength - anchor.minus(start).length()) * 0.35)
            function pointAt(t) { return start.plus(delta.times(t)).plus(Qt.vector3d(0, -sag * Math.sin(t * Math.PI), 0)); }
            function strandRotation(direction) {
                const d = direction.normalized();
                const length = Math.hypot(1 + d.y, d.z, d.x);
                return length < 0.0001 ? Qt.quaternion(0, 1, 0, 0)
                                      : Qt.quaternion((1 + d.y) / length, d.z / length, 0, -d.x / length);
            }
            PrincipledMaterial { id: silk; lighting: PrincipledMaterial.NoLighting; baseColor: "#ebf4f8" }
            Repeater3D {
                model: 16
                delegate: Model {
                    required property int index
                    readonly property vector3d begin: web.pointAt(index / 16)
                    readonly property vector3d end: web.pointAt((index + 1) / 16)
                    readonly property vector3d direction: end.minus(begin)
                    readonly property real length: Math.max(0.001, direction.length())
                    source: "#Cylinder"
                    position: begin.plus(end).times(0.5)
                    scale: Qt.vector3d(0.004, length / 100, 0.004)
                    rotation: web.strandRotation(direction)
                    materials: silk
                    castsShadows: false
                }
            }
            Model {
                source: "#Sphere"
                position: web.anchor
                scale: Qt.vector3d(0.09, 0.09, 0.09)
                visible: world.motion.shot >= 1
                materials: PrincipledMaterial { lighting: PrincipledMaterial.NoLighting; baseColor: "#87e6f2" }
            }
        }
    }

    View3D {
        id: handsView
        anchors.fill: parent
        visible: !world.overview
        explicitTextureWidth: view.explicitTextureWidth
        explicitTextureHeight: view.explicitTextureHeight
        environment: SceneEnvironment {
            antialiasingMode: SceneEnvironment.MSAA
            antialiasingQuality: SceneEnvironment.Medium
        }
        camera: PerspectiveCamera { fieldOfView: walkCamera.fieldOfView; clipNear: 2; clipFar: 200 }
        DirectionalLight { eulerRotation: Qt.vector3d(-28, -55, 0); brightness: 1.2; ambientColor: "#c4cbd4" }
        FirstPersonHands {
            id: hands
            walkPhase: world.walkPhase
            movementSpeed: world.velocity.length()
            airborne: !world.motion.grounded
            webActive: !!world.motion.anchor
            turnSway: Math.max(-8, Math.min(8, world.targetYaw - world.yaw))
            webDirection: world.motion.anchor
                ? walkCamera.mapPositionFromScene(Qt.vector3d(world.motion.anchor[0], world.motion.anchor[1], world.motion.anchor[2]))
                : Qt.vector3d(0, 0.3, -1)
        }
    }

    MouseArea {
        id: inputArea
        anchors.fill: parent
        acceptedButtons: Qt.LeftButton | Qt.MiddleButton
        hoverEnabled: true
        cursorShape: world.hovered?.workspaceId ? Qt.PointingHandCursor : world.hovered?.webSurface ? Qt.CrossCursor : Qt.ArrowCursor
        onPositionChanged: mouse => { pointer.x = mouse.x; pointer.y = mouse.y; }
        onPressed: mouse => {
            world.forceActiveFocus();
            const hit = view.pick(mouse.x, mouse.y).objectHit;
            if (mouse.button === Qt.MiddleButton || !hit || !("workspaceId" in hit)) {
                world.mouseWeb = true;
                world.fireWeb(true);
            }
        }
        onReleased: {
            world.mouseWeb = false;
            if (!world.heldKeys[Qt.Key_F]) world.releaseWeb();
        }
        onCanceled: { world.mouseWeb = false; world.releaseWeb(); }
        onClicked: mouse => { if (mouse.button === Qt.LeftButton) world.pickAt(mouse.x, mouse.y); }
    }
    QtObject { id: pointer; property real x: 0; property real y: 0 }
    Timer {
        interval: 80; running: world.activeFocus; repeat: true
        onTriggered: world.hovered = view.pick(pointer.x, pointer.y).objectHit
    }
    DragHandler {
        id: lookDrag
        target: null
        enabled: !trip.running
        acceptedButtons: Qt.RightButton
        property real startYaw
        property real startPitch
        onActiveChanged: if (active) {
            world.forceActiveFocus();
            startYaw = world.targetYaw;
            startPitch = world.targetPitch;
        }
        onActiveTranslationChanged: if (active) {
            world.targetYaw = startYaw - activeTranslation.x * 0.16;
            world.targetPitch = Math.max(-75, Math.min(70, startPitch - activeTranslation.y * 0.12));
        }
    }

    Rectangle {
        anchors.horizontalCenter: parent.horizontalCenter
        anchors.bottom: parent.bottom
        anchors.bottomMargin: 28
        width: controls.implicitWidth + 40
        height: controls.implicitHeight + 24
        radius: 12
        color: "#cc101b27"
        visible: world.showControls
        Text {
            id: controls
            anchors.centerIn: parent
            text: "WASD  move   ·   SHIFT  run / reel   ·   SPACE  jump / release\nHold F / middle mouse  web swing   ·   Click window  open\nQ / E or right-drag  look   ·   R  reset   ·   H  help   ·   ESC  exit"
            horizontalAlignment: Text.AlignHCenter
            color: "#d6e9f2"
            font { family: "GeistMono Nerd Font Mono"; pixelSize: 13 }
            lineHeight: 1.5
        }
    }

    Text {
        anchors.horizontalCenter: parent.horizontalCenter
        anchors.bottom: parent.bottom
        anchors.bottomMargin: world.showControls ? 140 : 35
        text: world.feedback
        color: "#d6e9f2"
        font { family: "GeistMono Nerd Font Mono"; pixelSize: 14 }
    }

}
