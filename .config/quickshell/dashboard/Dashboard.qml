pragma ComponentBehavior: Bound
import QtQuick
import QtQuick.Controls
import QtQuick.Layouts

Item {
    id: view
    property var snapshot: ({})
    property var preferences: ({city: ""})
    property string error: ""
    property date now: new Date()
    property var stories: []
    property var forecast: ({})
    readonly property string font: "DejaVu Sans"
    readonly property color ink: "#e7f2ff"
    readonly property color muted: "#b0c7dc"
    readonly property color cyan: "#4ee6e8"
    readonly property real zoom: Math.min(width / 1920, height / 1032)
    signal save(var arguments)
    focus: true

    function number(value, signed) {
        return (signed && value > 0 ? "+" : "") + Number(value).toLocaleString(Qt.locale("en_US"), "f", 2);
    }
    function condition(code) {
        if (code === 0) return "Clear skies";
        if (code === 1) return "Mostly clear";
        if (code === 2) return "Partly cloudy";
        if (code === 3) return "Overcast";
        if (code < 50) return "Fog";
        if (code < 60) return "Drizzle";
        if (code < 70) return "Rain";
        if (code < 80) return "Snow";
        if (code < 90) return "Showers";
        return "Thunderstorms";
    }
    function weatherIcon(code) { return code < 2 ? "☀" : code < 50 ? "☁" : code < 70 ? "☂" : code < 80 ? "❄" : "☂"; }
    function marketLabel(row) {
        let label;
        if (row.kind === "crypto") label = "24h · USD";
        else {
            const session = new Date(row.sessionDate + "T12:00:00");
            const weekend = now.getDay() === 0 || now.getDay() === 6;
            label = weekend && session.getDay() === 5 ? "Friday close" : Qt.formatDate(session, "MMM d") + " · Session";
        }
        if (now.getTime() - new Date(row.asOf).getTime() > 120000) label += " · Cached";
        return label;
    }
    function bandwidth(value) { return value > 1048576 ? (value / 1048576).toFixed(1) + " MB/s" : Math.round((value || 0) / 1024) + " KB/s"; }
    onSnapshotChanged: {
        const news = snapshot.news?.items || [];
        if (JSON.stringify(news) !== JSON.stringify(stories)) stories = news;
        if (JSON.stringify(snapshot.weather) !== JSON.stringify(forecast)) forecast = snapshot.weather || {};
        for (const row of snapshot.markets || []) {
            let index = -1;
            for (let i = 0; i < quotes.count; i++) if (quotes.get(i).symbol === row.symbol) index = i;
            if (index < 0) {
                const values = JSON.parse(JSON.stringify(row));
                values.spark = JSON.stringify(row.spark);
                quotes.append(values);
            }
            else for (const key of ["price", "change", "percent", "sessionDate", "asOf", "spark"]) {
                const value = key === "spark" ? JSON.stringify(row[key]) : row[key];
                if (quotes.get(index)[key] !== value) quotes.setProperty(index, key, value);
            }
        }
    }
    Timer { interval: 1000; running: view.visible; repeat: true; onTriggered: view.now = new Date() }
    ListModel { id: quotes; dynamicRoles: true }

    component Label: Text {
        color: view.ink
        font.family: view.font
        font.pixelSize: 18
        renderType: Text.NativeRendering
        style: Text.Raised
        styleColor: "#500c1830"
    }
    component FadeValue: Item {
        id: valueItem
        property string value: ""
        property string shown: ""
        property color color: view.ink
        property int pixelSize: 24
        property bool ready: false
        Component.onCompleted: { shown = value; ready = true; }
        onValueChanged: {
            if (!ready) shown = value;
            else if (shown !== value) { fade.stop(); fade.start(); }
        }
        Label {
            id: valueLabel
            objectName: "fadingLabel"
            anchors.fill: parent
            text: valueItem.shown
            color: valueItem.color
            font.pixelSize: valueItem.pixelSize
            verticalAlignment: Text.AlignVCenter
            Behavior on color { ColorAnimation { duration: 240 } }
        }
        SequentialAnimation {
            id: fade
            NumberAnimation { target: valueLabel; property: "opacity"; to: 0; duration: 130 }
            ScriptAction { script: valueItem.shown = valueItem.value }
            NumberAnimation { target: valueLabel; property: "opacity"; to: 1; duration: 180 }
        }
    }
    Item {
        id: canvas
        width: 1920; height: 1032
        anchors.centerIn: parent
        scale: view.zoom

        Label { x: 76; y: 74; text: "GOOD " + (view.now.getHours() < 12 ? "MORNING" : view.now.getHours() < 18 ? "AFTERNOON" : "EVENING") + ", PRAYUJ"; color: view.cyan; font.pixelSize: 13; font.letterSpacing: 2.4 }
        Label { x: 65; y: 101; text: Qt.formatTime(view.now, "h:mm"); font.pixelSize: 152; font.weight: Font.Light; font.letterSpacing: -7 }
        Label { x: 77; y: 292; text: Qt.formatDate(view.now, "dddd, MMMM d") + "  ·  " + Qt.formatTime(view.now, "AP"); color: view.muted; font.pixelSize: 20 }

        Item {
            x: 77; y: 380; width: 770; height: 245
            Label { y: 0; text: view.forecast.current ? Math.round(view.forecast.current.temperature_2m) + "°" : "—"; font.pixelSize: 58; font.weight: Font.Light }
            Label { x: 127; y: 11; text: view.forecast.current ? view.condition(view.forecast.current.weather_code) : view.forecast.error || "Loading forecast…"; font.pixelSize: 20 }
            Label { x: 128; y: 44; text: view.forecast.current ? "Feels like " + Math.round(view.forecast.current.apparent_temperature) + "°F" : ""; font.pixelSize: 13; color: view.muted }
            ToolButton {
                id: locationButton
                objectName: "changeCity"
                x: -10; y: 77; height: 35
                text: (view.preferences.city || "Choose a city") + "  ⌄"
                contentItem: Label { text: locationButton.text; color: locationButton.hovered ? view.cyan : view.muted; font.pixelSize: 13; verticalAlignment: Text.AlignVCenter }
                background: Item {}
                onClicked: weatherDialog.open()
            }
            Row {
                y: 137; spacing: 22
                Repeater {
                    model: view.forecast.hours || []
                    Column {
                        required property var modelData
                        width: 90; spacing: 8
                        Label { width: 90; horizontalAlignment: Text.AlignHCenter; text: Qt.formatTime(new Date(parent.modelData.time), "h AP"); font.pixelSize: 11; color: view.muted }
                        Label { width: 90; horizontalAlignment: Text.AlignHCenter; text: view.weatherIcon(parent.modelData.code); font.pixelSize: 24; color: view.cyan }
                        Label { width: 90; horizontalAlignment: Text.AlignHCenter; text: Math.round(parent.modelData.temperature) + "°"; font.pixelSize: 20 }
                        Label { width: 90; horizontalAlignment: Text.AlignHCenter; text: parent.modelData.rain + "% rain"; font.pixelSize: 10; color: view.muted }
                    }
                }
            }
            Label { y: 266; text: view.forecast.error || "Open-Meteo · Next six hours"; font.pixelSize: 10; color: view.muted; opacity: 0.7 }
        }

        Item {
            x: 1095; y: 117; width: 749; height: 615
            Label { text: "THE LATEST"; color: view.cyan; font.pixelSize: 13; font.letterSpacing: 2.4 }
            Label { x: 512; y: 2; text: "NYT · Politics / Economy"; color: view.muted; font.pixelSize: 11 }
            Flickable {
                id: newsFeed
                objectName: "newsFeed"
                y: 46; width: 749; height: 554
                contentHeight: newsRows.height; clip: true; boundsBehavior: Flickable.StopAtBounds
                // A direct write with the behavior disabled cancels the wheel glide.
                onMovementStarted: contentY = contentY
                ScrollBar.vertical: ScrollBar {
                    id: newsScroll
                    width: 3; policy: ScrollBar.AsNeeded
                    onPressedChanged: if (pressed) newsFeed.contentY = newsFeed.contentY
                    contentItem: Rectangle { implicitWidth: 3; radius: 2; color: view.cyan; opacity: newsScroll.active ? 0.65 : 0.15 }
                    background: Item {}
                }
                WheelHandler {
                    id: newsWheel
                    target: null
                    acceptedDevices: PointerDevice.Mouse | PointerDevice.TouchPad
                    property real destination: 0
                    onWheel: event => {
                        newsFeed.cancelFlick();
                        const delta = event.pixelDelta.y ? event.pixelDelta.y * 3 : event.angleDelta.y * 0.8;
                        if (!delta) return;
                        const continuing = scroll.running && delta * (destination - newsFeed.contentY) < 0;
                        const start = continuing ? destination : newsFeed.contentY;
                        const end = Math.max(0, Math.min(Math.max(0, newsFeed.contentHeight - newsFeed.height), start - delta));
                        destination = end;
                        newsFeed.contentY = end;
                        event.accepted = true;
                    }
                }
                Behavior on contentY {
                    enabled: newsWheel.active && !newsFeed.moving && !newsScroll.pressed
                    SmoothedAnimation {
                        id: scroll; duration: 160; velocity: -1; maximumEasingTime: 60
                        reversingMode: SmoothedAnimation.Immediate
                    }
                }
                Column {
                    id: newsRows
                    width: 724; spacing: 23
                    Repeater {
                        model: view.stories
                        Item {
                            id: article
                            required property var modelData
                            readonly property bool hasImage: !!modelData.image && thumbnail.status !== Image.Error
                            width: 724; height: 124
                            Image { id: thumbnail; y: 2; width: 164; height: 116; visible: article.hasImage; source: article.modelData.image; sourceSize: Qt.size(328, 232); fillMode: Image.PreserveAspectCrop; asynchronous: true }
                            Label { x: article.hasImage ? 185 : 0; y: 0; text: article.modelData.section.toUpperCase(); color: view.muted; font.pixelSize: 10; font.letterSpacing: 1.4 }
                            Label { x: article.hasImage ? 184 : 0; y: 25; width: article.width - x - 2; height: 96; text: article.modelData.title; font.pixelSize: 21; wrapMode: Text.WordWrap; maximumLineCount: 3; elide: Text.ElideRight; color: articleMouse.containsMouse ? view.cyan : view.ink; Behavior on color { ColorAnimation { duration: 130 } } }
                            MouseArea { id: articleMouse; anchors.fill: parent; hoverEnabled: true; cursorShape: Qt.PointingHandCursor; onClicked: Qt.openUrlExternally(article.modelData.link) }
                        }
                    }
                    Label { visible: !view.stories.length; text: "Waiting for the news feed…"; color: view.muted }
                }
            }
            Label { y: 609; text: "Scroll to explore  ·  Click to read"; color: view.muted; font.pixelSize: 10; opacity: 0.7 }
        }

        Label { x: 77; y: 790; text: "MARKETS"; color: view.cyan; font.pixelSize: 12; font.letterSpacing: 2 }
        Label { x: 1320; y: 791; text: "Yahoo Finance / Coinbase · ~10s · Recent samples"; color: view.muted; font.pixelSize: 10; opacity: 0.8 }
        Row {
            x: 77; y: 824; spacing: 31
            Repeater {
                model: quotes
                Item {
                    id: market
                    required property string name
                    required property string symbol
                    required property string kind
                    required property real price
                    required property real change
                    required property real percent
                    required property string sessionDate
                    required property string asOf
                    required property string spark
                    readonly property color movement: change < 0 ? "#f28b68" : "#b0e86f"
                    objectName: "market" + symbol
                    width: 270; height: 130
                    Label { text: market.name; font.pixelSize: 14 }
                    Label { y: 25; text: view.marketLabel(market); color: view.muted; font.pixelSize: 10 }
                    FadeValue { objectName: "price" + market.symbol; y: 43; width: 265; height: 34; value: (market.kind === "crypto" ? "$" : "") + view.number(market.price, false); pixelSize: 26 }
                    FadeValue { y: 84; width: 265; height: 20; value: view.number(market.percent, true) + "%  ·  " + view.number(market.change, true) + (market.kind === "crypto" ? " USD" : " pts"); color: market.movement; pixelSize: 12 }
                    Canvas {
                        id: sparkline
                        y: 111; width: 248; height: 22; opacity: 0.85
                        onPaint: {
                            const points = JSON.parse(market.spark);
                            const ctx = getContext("2d"); ctx.reset();
                            if (!points || points.length < 2) return;
                            const low = Math.min(...points), high = Math.max(...points);
                            ctx.beginPath(); ctx.strokeStyle = market.movement; ctx.lineWidth = 1.4;
                            for (let i = 0; i < points.length; i++) {
                                const x = i * width / (points.length - 1);
                                const y = high === low ? height / 2 : height - 2 - (points[i] - low) * (height - 4) / (high - low);
                                if (i === 0) ctx.moveTo(x, y); else ctx.lineTo(x, y);
                            }
                            ctx.stroke();
                        }
                        Connections { target: market; function onSparkChanged() { sparkline.requestPaint(); } function onMovementChanged() { sparkline.requestPaint(); } }
                    }
                }
            }
        }
        Row {
            x: 77; y: 986; spacing: 27
            Label { text: "CPU  " + Math.round(view.snapshot.stats?.cpu || 0) + "%  /  " + Math.round(view.snapshot.stats?.cpuTemperature || 0) + "°C"; color: view.muted; font.pixelSize: 11 }
            Label { text: "GPU  " + (view.snapshot.stats?.gpu?.temperature === undefined ? "—" : Math.round(view.snapshot.stats.gpu.temperature) + "°C  /  " + Math.round(view.snapshot.stats.gpu.load) + "%"); color: view.muted; font.pixelSize: 11 }
            Label { text: "RAM  " + (view.snapshot.stats?.memoryUsed || 0).toFixed(1) + " / " + Math.round(view.snapshot.stats?.memoryTotal || 0) + " GB"; color: view.muted; font.pixelSize: 11 }
            Label { text: (view.snapshot.stats?.network === "Offline" ? "Offline" : view.snapshot.stats ? "Online" : "Connecting") + "  ↓ " + view.bandwidth(view.snapshot.stats?.download) + "  ↑ " + view.bandwidth(view.snapshot.stats?.upload); color: view.muted; font.pixelSize: 11 }
            Label { text: Math.round(view.snapshot.stats?.diskFree || 0) + " GB free"; color: view.muted; font.pixelSize: 11 }
        }
        Label { x: 1594; y: 986; text: "SUPER + SHIFT + W to return"; color: view.muted; font.pixelSize: 10; opacity: 0.8 }
        Label { x: 77; y: 1010; text: view.error; color: "#f28b68"; font.pixelSize: 11 }
    }
    Dialog {
        id: weatherDialog
        objectName: "weatherDialog"
        anchors.centerIn: parent
        width: 420; modal: true; title: "Weather location"
        palette.windowText: view.ink
        background: Rectangle { color: "#101a2b"; radius: 14; border.color: "#355578" }
        onOpened: { city.text = view.preferences.city || ""; city.forceActiveFocus(); }
        function commit() {
            if (!city.text.trim()) return;
            view.save(["city", city.text.trim()]);
            close();
        }
        ColumnLayout {
            width: parent.width; spacing: 18
            TextField {
                id: city
                objectName: "weatherCity"
                Layout.fillWidth: true
                placeholderText: "City, state or country"
                font.family: view.font; font.pixelSize: 15
                color: view.ink; placeholderTextColor: view.muted
                selectionColor: "#355578"
                background: Rectangle { implicitHeight: 42; color: "#202e43"; radius: 6; border.color: city.activeFocus ? view.cyan : "#355578" }
                onAccepted: weatherDialog.commit()
            }
            Button {
                id: applyCity
                objectName: "saveCity"
                text: "Use this location"; font.family: view.font
                contentItem: Label { text: applyCity.text; font.pixelSize: 13; horizontalAlignment: Text.AlignHCenter; verticalAlignment: Text.AlignVCenter }
                background: Rectangle { implicitWidth: 180; implicitHeight: 36; radius: 6; color: applyCity.down ? "#355578" : "#202e43" }
                onClicked: weatherDialog.commit()
            }
        }
    }
}
