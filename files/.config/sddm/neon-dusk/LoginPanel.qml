import QtQuick
import QtQuick.Controls

Item {
    id: panel
    property bool authenticating: false
    property string message: ""
    property date now: new Date()
    readonly property real uiScale: Math.min(width / 1920, height / 1080)

    function submit() {
        if (authenticating || password.text.length === 0 || sessionPicker.currentIndex < 0)
            return;
        message = "";
        authenticating = true;
        sddm.login(config.username, password.text, sessionPicker.currentIndex);
        password.text = "";
    }

    Timer { interval: 1000; running: true; repeat: true; onTriggered: panel.now = new Date() }
    Connections {
        target: sddm
        function onLoginFailed() {
            panel.authenticating = false;
            panel.message = "That password didn’t work. Try again.";
            password.forceActiveFocus();
        }
        function onLoginSucceeded() { panel.message = "Welcome back."; }
        function onInformationMessage(message) { panel.message = message; }
    }

    Item {
        width: 1920
        height: 1080
        anchors.centerIn: parent
        scale: panel.uiScale

        Column {
            anchors.horizontalCenter: parent.horizontalCenter
            y: 218
            spacing: 14

            Text {
                anchors.horizontalCenter: parent.horizontalCenter
                text: Qt.formatTime(panel.now, "hh:mm")
                color: "#f3f8ff"
                font.family: config.clockFont
                font.pixelSize: 174
                font.weight: Font.Light
                font.letterSpacing: -10
            }
            Text {
                anchors.horizontalCenter: parent.horizontalCenter
                text: Qt.formatDate(panel.now, "dddd, MMMM d")
                color: "#d7e5f3"
                font.family: config.font
                font.pixelSize: 26
                font.letterSpacing: 0.5
            }
        }

        Column {
            width: 390
            anchors.horizontalCenter: parent.horizontalCenter
            y: 586
            spacing: 18

            Text {
                anchors.horizontalCenter: parent.horizontalCenter
                text: config.displayName
                color: "#edf5ff"
                font.family: config.font
                font.pixelSize: 28
            }

            Rectangle {
                width: parent.width
                height: 62
                radius: 20
                color: password.activeFocus ? "#99121e33" : "#73121e33"
                border.width: 1
                border.color: password.activeFocus ? "#aa75ccd9" : "#594e6985"
                Behavior on border.color { ColorAnimation { duration: 140 } }

                TextField {
                    id: password
                    objectName: "password"
                    anchors.left: parent.left
                    anchors.right: enterButton.left
                    anchors.verticalCenter: parent.verticalCenter
                    anchors.leftMargin: 20
                    height: 54
                    color: "#f3f8ff"
                    placeholderText: "Password"
                    placeholderTextColor: "#a4b6cb"
                    echoMode: TextInput.Password
                    passwordCharacter: "•"
                    font.family: config.font
                    font.pixelSize: 20
                    background: Item {}
                    selectByMouse: true
                    enabled: !panel.authenticating
                    focus: true
                    onAccepted: panel.submit()
                    Keys.onEscapePressed: { text = ""; panel.message = ""; }
                    Component.onCompleted: forceActiveFocus()
                }
                Button {
                    id: enterButton
                    objectName: "submit"
                    anchors.right: parent.right
                    anchors.rightMargin: 9
                    anchors.verticalCenter: parent.verticalCenter
                    width: 44
                    height: 44
                    enabled: !panel.authenticating && password.text.length > 0
                    onClicked: panel.submit()
                    contentItem: Text {
                        text: panel.authenticating ? "···" : "→"
                        color: enterButton.enabled ? "#edf9ff" : "#8196b1"
                        font.pixelSize: 24
                        horizontalAlignment: Text.AlignHCenter
                        verticalAlignment: Text.AlignVCenter
                    }
                    background: Rectangle {
                        radius: 14
                        color: enterButton.hovered ? "#536889" : "#31445e"
                    }
                }
            }
            Text {
                width: parent.width
                height: 46
                text: panel.message || (keyboard.capsLock ? "Caps Lock is on" :
                    panel.authenticating ? "Signing in…" : "Press Enter to sign in")
                textFormat: Text.PlainText
                horizontalAlignment: Text.AlignHCenter
                wrapMode: Text.WordWrap
                color: panel.message && !panel.authenticating ? "#f6c4b2" : "#bacbdf"
                font.family: config.font
                font.pixelSize: 15
            }
        }

        ComboBox {
            id: sessionPicker
            objectName: "sessionPicker"
            anchors.horizontalCenter: parent.horizontalCenter
            y: 956
            width: 264
            height: 38
            model: sessionModel
            textRole: "name"
            valueRole: "file"
            enabled: !panel.authenticating
            font.family: config.font
            font.pixelSize: 13
            palette.text: "#cad8e9"
            palette.buttonText: "#cad8e9"
            palette.window: "#18263c"
            palette.highlight: "#355578"
            background: Rectangle { color: "#4510182b"; radius: 12 }
            Component.onCompleted: {
                currentIndex = indexOfValue(config.session);
                console.info("Neon Dusk session:", currentIndex, currentValue);
            }
        }

        Row {
            anchors.right: parent.right
            anchors.bottom: parent.bottom
            anchors.margins: 38
            spacing: 12
            Repeater {
                model: [ { label: "Restart", action: "restart" }, { label: "Shut down", action: "shutdown" } ]
                Button {
                    id: powerButton
                    required property var modelData
                    width: modelData.action === "restart" ? 105 : 126
                    height: 42
                    visible: modelData.action === "restart" ? sddm.canReboot : sddm.canPowerOff
                    onClicked: modelData.action === "restart" ? sddm.reboot() : sddm.powerOff()
                    contentItem: Text {
                        text: powerButton.modelData.label
                        font.family: config.font
                        font.pixelSize: 14
                        color: "#d3dfed"
                        horizontalAlignment: Text.AlignHCenter
                        verticalAlignment: Text.AlignVCenter
                    }
                    background: Rectangle { radius: 13; color: powerButton.hovered ? "#66354b68" : "#3710182b" }
                }
            }
        }
    }
}
