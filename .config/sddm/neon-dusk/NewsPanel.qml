import QtQuick
import QtQuick.Controls
import QtQuick.Layouts

Item {
    id: panel
    objectName: "newsPanel"
    property var stories: []
    property date updated: new Date(0)
    property date now: new Date()
    readonly property real uiScale: Math.min(width / 2560, height / 1440)
    function refresh() {
        const request = new XMLHttpRequest();
        request.open("GET", "file://" + config.newsCache);
        request.onreadystatechange = function() {
            if (request.readyState !== XMLHttpRequest.DONE)
                return;
            try {
                const data = JSON.parse(request.responseText);
                if (data.items.length > 0) {
                    // Preserve delegates and scroll position when only timestamps change.
                    if (JSON.stringify(data.items) !== JSON.stringify(panel.stories))
                        panel.stories = data.items;
                    panel.updated = new Date(data.updated);
                }
            } catch (error) {}
        };
        request.send();
    }
    Component.onCompleted: refresh()
    Timer { interval: 10000; running: true; repeat: true; onTriggered: { panel.now = new Date(); panel.refresh(); } }

    Item {
        width: 2560
        height: 1440
        anchors.centerIn: parent
        scale: panel.uiScale
        Text {
            x: 88; y: 68
            text: "THE DAILY BRIEF"
            color: "#ecf5ff"
            font.family: config.font
            font.pixelSize: 39
            font.weight: Font.DemiBold
            font.letterSpacing: 5
        }
        Text {
            x: 91; y: 130
            text: "U.S. POLITICS  /  ECONOMY"
            color: "#8cccd8"
            font.family: config.font
            font.pixelSize: 15
            font.letterSpacing: 3
        }
        Text {
            anchors.right: parent.right
            anchors.rightMargin: 88
            y: 82
            text: Qt.formatDate(panel.now, "dddd, MMMM d")
            color: "#b5c6da"
            font.family: config.font
            font.pixelSize: 21
        }
        Rectangle { x: 88; y: 184; width: 2384; height: 1; color: "#45617d" }
        Flickable {
            id: newsFeed
            objectName: "newsFeed"
            x: 88; y: 221
            width: 1692; height: 1095
            contentWidth: width
            contentHeight: newsContent.height
            clip: true
            boundsBehavior: Flickable.StopAtBounds
            flickableDirection: Flickable.VerticalFlick
            ScrollBar.vertical: ScrollBar { policy: ScrollBar.AsNeeded; width: 8 }
            WheelHandler {
                target: null
                onWheel: event => {
                    newsFeed.cancelFlick();
                    const delta = event.pixelDelta.y || event.angleDelta.y * 0.8;
                    const position = wheelScroll.running ? wheelScroll.to : newsFeed.contentY;
                    wheelScroll.stop();
                    wheelScroll.to = Math.max(0, Math.min(newsFeed.contentHeight - newsFeed.height, position - delta));
                    wheelScroll.start();
                    event.accepted = true;
                }
            }
            NumberAnimation {
                id: wheelScroll
                target: newsFeed
                property: "contentY"
                duration: 220
                easing.type: Easing.OutCubic
            }
            Column {
                id: newsContent
                width: newsFeed.width - 18
                spacing: 24
                Rectangle {
                    id: hero
                    width: parent.width; height: 570
                    radius: 24
                    color: "#b8132136"
                    border.width: 1
                    border.color: "#3a62748b"
                    visible: panel.stories.length > 0
                    readonly property var story: panel.stories.length ? panel.stories[0] : ({})
                    Rectangle {
                        x: 15; y: 15; width: 786; height: 540
                        radius: 15
                        color: "#293c54"
                        clip: true
                        Image {
                            anchors.fill: parent
                            source: hero.story.image || ""
                            asynchronous: true
                            fillMode: Image.PreserveAspectCrop
                            sourceSize.width: 1050
                        }
                    }
                    Column {
                        x: 833; y: 39
                        width: 797
                        spacing: 21
                        Text {
                            text: "TOP STORY  ·  " + (hero.story.section || "Politics").toUpperCase()
                            color: "#87d7df"
                            font.family: config.font
                            font.pixelSize: 15
                            font.letterSpacing: 2
                        }
                        Text {
                            width: parent.width
                            text: hero.story.title || ""
                            textFormat: Text.PlainText
                            color: "#f0f6ff"
                            font.family: config.font
                            font.pixelSize: 37
                            font.weight: Font.DemiBold
                            wrapMode: Text.WordWrap
                            maximumLineCount: 6
                            elide: Text.ElideRight
                            lineHeight: 1.1
                        }
                        Text {
                            width: parent.width
                            text: hero.story.desc || ""
                            textFormat: Text.PlainText
                            color: "#b7c8dc"
                            font.family: config.font
                            font.pixelSize: 20
                            wrapMode: Text.WordWrap
                            maximumLineCount: 5
                            elide: Text.ElideRight
                            lineHeight: 1.2
                        }
                    }
                }
                GridLayout {
                    width: parent.width
                    columns: 2
                    columnSpacing: 24
                    rowSpacing: 24
                    Repeater {
                        model: panel.stories.slice(1)
                        Rectangle {
                            id: storyCard
                            required property var modelData
                            Layout.preferredWidth: (newsContent.width - 24) / 2
                            Layout.preferredHeight: 276
                            radius: 20
                            color: "#a6132136"
                            border.width: 1
                            border.color: "#32586b83"
                            Rectangle {
                                x: 13; y: 13; width: 239; height: 250
                                radius: 12
                                color: "#293c54"
                                clip: true
                                Image {
                                    anchors.fill: parent
                                    source: storyCard.modelData.image || ""
                                    asynchronous: true
                                    fillMode: Image.PreserveAspectCrop
                                    sourceSize.width: 550
                                }
                            }
                            Column {
                                x: 275; y: 25
                                width: storyCard.width - 300
                                spacing: 15
                                Text {
                                    text: (storyCard.modelData.section || "Politics").toUpperCase()
                                    color: "#87cbd7"
                                    font.family: config.font
                                    font.pixelSize: 12
                                    font.letterSpacing: 2
                                }
                                Text {
                                    width: parent.width
                                    text: storyCard.modelData.title
                                    textFormat: Text.PlainText
                                    color: "#e7f0fc"
                                    font.family: config.font
                                    font.pixelSize: 24
                                    font.weight: Font.DemiBold
                                    wrapMode: Text.WordWrap
                                    maximumLineCount: 6
                                    elide: Text.ElideRight
                                    lineHeight: 1.1
                                }
                            }
                        }
                    }
                }
            }
        }
        MarketPanel { x: 1816; y: 221; width: 656; height: 1095 }
        Text {
            x: 88; y: 1356
            text: "THE NEW YORK TIMES  ·  Scroll for more stories ↓"
            color: "#98afc9"
            font.family: config.font
            font.pixelSize: 13
            font.letterSpacing: 0.7
        }
        Text {
            anchors.right: parent.right
            anchors.rightMargin: 88
            y: 1356
            text: panel.updated.getTime() > 0 ? "News checked " + Qt.formatTime(panel.updated, "h:mm:ss AP") : "Waiting for an update"
            color: "#98afc9"
            font.family: config.font
            font.pixelSize: 13
        }
    }
}
