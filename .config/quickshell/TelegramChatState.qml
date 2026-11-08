import QtQuick

QtObject {
    id: chat
    required property string key
    required property string name
    property bool ready: false
    property string error: ""
    property string draft: ""
    property int cursorPosition: 0
    property string attachment: ""
    property string sendError: ""
    property bool sending: false
    property bool typing: false
    property bool followTail: true
    property real scrollY: 0
    property int scrollMessage: -1
    property real scrollOffset: 0
    property var seen: ({})
    property var historyUpdates: ({})
    readonly property ListModel messages: ListModel {}
    readonly property Timer typingTimeout: Timer {
        interval: 7000
        onTriggered: chat.typing = false
    }

    function messageRow(message) {
        return { mid: message.id === undefined ? -1 : message.id, out: message.out === true,
            text: message.text || "", hasImage: message.hasImage === true,
            image: message.image || "", imageError: message.imageError || "" }
    }

    function stopTyping() {
        typing = false
        typingTimeout.stop()
    }

    function disconnected() {
        ready = false
        historyUpdates = ({})
        stopTyping()
        if (sending) {
            sending = false
            sendError = "Connection closed. Your draft is saved; try again."
        }
    }

    // Return true only for a new incoming message, so duplicate events cannot notify twice.
    function applyEvent(event) {
        if (event.type === "history") {
            // Live events can arrive while history is loading. Preserve those messages
            // and any images already downloaded instead of replacing them with stale rows.
            let live = []
            for (let i = 0; i < messages.count; i++) live.push(messageRowFromModel(i))
            messages.clear()
            seen = ({})
            let rows = []
            for (let message of event.messages) {
                seen[message.id] = true
                let existing = live.find(row => row.mid === message.id)
                let row = historyUpdates[message.id] && existing ? existing : messageRow(message)
                if (existing && row.hasImage && !row.image) row.image = existing.image
                rows.push(row)
            }
            for (let row of live) {
                if (!seen[row.mid]) {
                    seen[row.mid] = true
                    rows.push(row)
                }
            }
            rows.sort((a, b) => a.mid - b.mid)
            for (let row of rows) messages.append(row)
            historyUpdates = ({})
        } else if (event.type === "msg" || event.type === "edit") {
            stopTyping()
            historyUpdates[event.id] = true
            if (!seen[event.id]) {
                seen[event.id] = true
                messages.append(messageRow(event))
                return event.type === "msg" && !event.out && !!(event.text || event.hasImage)
            }
            if (event.type === "edit") {
                for (let i = messages.count - 1; i >= 0; i--) {
                    if (messages.get(i).mid === event.id) {
                        messages.setProperty(i, "text", event.text || "")
                        messages.setProperty(i, "hasImage", event.hasImage === true)
                        break
                    }
                }
            }
        } else if (event.type === "image") {
            for (let i = messages.count - 1; i >= 0; i--) {
                if (messages.get(i).mid === event.id) {
                    messages.setProperty(i, "image", event.image || "")
                    messages.setProperty(i, "imageError", event.error || "")
                    break
                }
            }
        } else if (event.type === "send-result") {
            sending = false
            sendError = event.error || ""
            if (event.ok) {
                draft = ""
                cursorPosition = 0
                attachment = ""
            }
        } else if (event.type === "typing") {
            typing = event.on === true
            if (typing) typingTimeout.restart()
            else typingTimeout.stop()
        } else if (event.type === "status") {
            ready = event.ok === true
            error = event.error || ""
        }
        return false
    }

    function messageRowFromModel(index) {
        let row = messages.get(index)
        return { mid: row.mid, out: row.out, text: row.text, hasImage: row.hasImage,
            image: row.image, imageError: row.imageError }
    }
}
