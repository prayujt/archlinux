import QtQuick

Item {
    id: globe
    required property var now
    readonly property double epoch: Date.now()
    property real spinAngle: -0.3
    readonly property var sun: solarPosition(now)
    readonly property vector3d sunDirection: Qt.vector3d(sun.x, sun.y, sun.z)
    readonly property bool ready: dayMap.status === Image.Ready && cloudMap.status === Image.Ready
        && nightMap.status === Image.Ready && dayCities.status === Image.Ready

    // NOAA's approximate solar position, in Earth-fixed geographic coordinates.
    function solarPosition(date: var): var {
        const year = date.getUTCFullYear();
        const yearStart = Date.UTC(year, 0, 1);
        const daysInYear = (Date.UTC(year + 1, 0, 1) - yearStart) / 86400000;
        const day = Math.floor((Date.UTC(year, date.getUTCMonth(), date.getUTCDate()) - yearStart) / 86400000) + 1;
        const hours = date.getUTCHours() + date.getUTCMinutes() / 60 + date.getUTCSeconds() / 3600;
        const gamma = 2 * Math.PI / daysInYear * (day - 1 + (hours - 12) / 24);
        const equation = 229.18 * (0.000075 + 0.001868 * Math.cos(gamma) - 0.032077 * Math.sin(gamma)
            - 0.014615 * Math.cos(2 * gamma) - 0.040849 * Math.sin(2 * gamma));
        const declination = 0.006918 - 0.399912 * Math.cos(gamma) + 0.070257 * Math.sin(gamma)
            - 0.006758 * Math.cos(2 * gamma) + 0.000907 * Math.sin(2 * gamma)
            - 0.002697 * Math.cos(3 * gamma) + 0.00148 * Math.sin(3 * gamma);
        const longitude = (720 - hours * 60 - equation) / 4 * Math.PI / 180;
        return {x: Math.cos(declination) * Math.sin(longitude), y: Math.sin(declination),
            z: Math.cos(declination) * Math.cos(longitude),
            latitude: declination * 180 / Math.PI, longitude: longitude * 180 / Math.PI};
    }
    Timer {
        interval: 50; repeat: true; running: globe.visible
        onTriggered: globe.spinAngle = -0.3 + (Date.now() - globe.epoch) / 360000 * Math.PI * 2
    }
    Image {
        id: dayMap
        visible: false; smooth: true; mipmap: true
        source: "file:///home/prayuj/.local/share/desktop-styles/nothing-orbit/assets/earth-day.jpg"
        sourceSize.width: 4096
    }
    Image {
        id: cloudMap
        visible: false; smooth: true; mipmap: true
        source: "file:///home/prayuj/.local/share/desktop-styles/nothing-orbit/assets/earth-clouds.jpg"
    }
    Image {
        id: nightMap
        visible: false; smooth: true; mipmap: true
        source: "file:///home/prayuj/.local/share/desktop-styles/nothing-orbit/assets/earth-night.jpg"
        sourceSize.width: 4096
    }
    ShaderEffect {
        anchors.fill: parent
        property var dayTexture: dayMap
        property var cloudTexture: cloudMap
        property var nightTexture: nightMap
        property var dayMarkerTexture: dayCities
        property real phase: globe.spinAngle
        property vector3d sunDirection: globe.sunDirection
        fragmentShader: "file:///home/prayuj/.local/share/desktop-styles/nothing-orbit/assets/earth-sunlight.frag.qsb"
    }
    Image {
        id: dayCities
        visible: false; smooth: true; mipmap: true
        source: "file:///home/prayuj/.local/share/desktop-styles/nothing-orbit/assets/earth-major-cities.svg"
        sourceSize.width: 4096
    }
}
