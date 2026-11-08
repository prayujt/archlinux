import QtQuick

QtObject {
    id: toast
    property string app: ""
    property string text: ""
    property string body: ""
    property string source: ""
    property int serial: 0
    property bool shown: false
    property bool important: false
    property bool hovered: false
    property string error: ""
    property string agentAttention: "none"
    readonly property bool needsUser: agentAttention === "approval" || agentAttention === "input"
    property int nextSerial: 0
    property var notifications: ({})
    property var pending: ({})
    property var waiting: []
    signal classify(var notification)

    property Timer expiry: Timer {
        interval: toast.important ? 20000 : 10000
        onTriggered: toast.shown = false
    }
    property Timer debounce: Timer {
        interval: 500
        onTriggered: {
            let batch = toast.pending
            toast.pending = ({})
            for (let key of Object.keys(batch)) {
                let notification = batch[key]
                if (!notification.dismissed) toast.classify(notification)
            }
        }
    }
    onHoveredChanged: restartExpiry()

    function restartExpiry() {
        if (shown && !hovered && !needsUser) expiry.restart()
        else expiry.stop()
    }
    function remember(notification) {
        notifications[notification.source] = notification
        pending[notification.source] = notification
        // Retain recent streams so an approval arriving in a late edit can still notify.
        let keys = Object.keys(notifications)
        if (keys.length > 50) {
            let oldest = keys.find(key => waiting.indexOf(key) < 0 && key !== notification.source)
            if (oldest !== undefined) delete notifications[oldest]
        }
        debounce.restart()
    }
    function display(notification) {
        serial = notification.id
        app = notification.app
        text = notification.summary
        body = notification.body
        source = notification.source
        agentAttention = notification.agentAttention
        important = notification.important || needsUser
        error = notification.error || ""
        shown = true
        restartExpiry()
    }
    function show(appName, summary, details, sourceId) {
        let notification = {id: ++nextSerial, app: appName, summary: summary || "",
            body: details || "", source: sourceId || "toast:" + nextSerial,
            important: false, agentAttention: "none", dismissed: false}
        remember(notification)
        if (!(shown && needsUser)) display(notification)
        return notification.id
    }
    function update(sourceId, summary) {
        let previous = notifications[sourceId]
        if (!previous || previous.dismissed || previous.summary === summary) return
        let notification = Object.assign({}, previous, {id: ++nextSerial, summary: summary})
        remember(notification)
        if (shown && source === sourceId) display(notification)
    }
    function applyClassification(result) {
        let notification = Object.values(notifications).find(entry => entry.id === result.id)
        // Edits and acknowledgement invalidate old decisions; expiry still permits new approval alerts.
        if (!notification || notification.dismissed) return
        let wasWaiting = shown && needsUser && source === notification.source
        notification.error = result.error || ""
        // A network failure cannot clear an already recognized approval request.
        if (!result.error) {
            notification.agentAttention = ["approval", "input"].indexOf(result.agentAttention) >= 0
                ? result.agentAttention : "none"
            notification.important = result.important === true
        }
        let needsAttention = ["approval", "input"].indexOf(notification.agentAttention) >= 0
        waiting = waiting.filter(key => key !== notification.source)
        if (needsAttention) waiting.push(notification.source)
        if (needsAttention && !(shown && needsUser && source !== notification.source)) {
            display(notification)
        } else if (wasWaiting && !needsAttention) {
            if (waiting.length) display(notifications[waiting[0]])
            else { shown = false; agentAttention = "none"; expiry.stop() }
        } else if (shown && source === notification.source) display(notification)
    }
    function dismiss() {
        if (notifications[source]) notifications[source].dismissed = true
        waiting = waiting.filter(key => key !== source)
        expiry.stop()
        if (waiting.length) display(notifications[waiting[0]])
        else shown = false
    }
    function resolveChat(chat) {
        let prefix = chat + ":"
        for (let notification of Object.values(notifications)) {
            if (notification.source.indexOf(prefix) === 0) notification.dismissed = true
        }
        waiting = waiting.filter(key => key.indexOf(prefix) !== 0)
        if (source.indexOf(prefix) === 0) {
            expiry.stop()
            if (waiting.length) display(notifications[waiting[0]])
            else shown = false
        }
    }
}
