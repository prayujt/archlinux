import QtQuick

Item {
    id: valueItem
    property string value: ""
    property string shown: value
    property color color: "#eef5ff"
    property int pixelSize: 32
    property int fontWeight: Font.Normal
    property int alignment: Text.AlignLeft
    property bool ready: false
    onValueChanged: {
        if (!ready)
            shown = value;
        else if (value !== shown) {
            fade.stop();
            fade.start();
        }
    }
    Component.onCompleted: { shown = value; ready = true; }
    Text {
        id: label
        objectName: "valueLabel"
        anchors.fill: parent
        text: valueItem.shown
        color: valueItem.color
        font.family: config.font
        font.pixelSize: valueItem.pixelSize
        font.weight: valueItem.fontWeight
        horizontalAlignment: valueItem.alignment
        verticalAlignment: Text.AlignVCenter
        Behavior on color { ColorAnimation { duration: 250 } }
    }
    SequentialAnimation {
        id: fade
        NumberAnimation { target: label; property: "opacity"; to: 0; duration: 130 }
        ScriptAction { script: valueItem.shown = valueItem.value }
        NumberAnimation { target: label; property: "opacity"; to: 1; duration: 180 }
    }
}
