import QtQuick
import QtTest
import "../.."

TestCase {
    name: "TelegramChatIsolation"
    TelegramChatState { id: ray; key: "ray"; name: "Ray" }
    TelegramChatState { id: code; key: "ray-code"; name: "Ray Code" }

    function init() {
        for (let chat of [ray, code]) {
            chat.messages.clear()
            chat.seen = ({})
            chat.historyUpdates = ({})
            chat.draft = ""
            chat.attachment = ""
            chat.sendError = ""
            chat.sending = false
            chat.stopTyping()
        }
    }

    function test_same_message_id_stays_in_its_own_chat() {
        verify(ray.applyEvent({type: "msg", id: 42, text: "Ray reply", out: false}))
        verify(code.applyEvent({type: "msg", id: 42, text: "Code reply", out: false}))
        code.applyEvent({type: "edit", id: 42, text: "Code streamed reply", out: false, hasImage: true})
        code.applyEvent({type: "image", id: 42, image: "file:///tmp/code.png"})
        compare(ray.messages.count, 1)
        compare(ray.messages.get(0).text, "Ray reply")
        compare(ray.messages.get(0).image, "")
        compare(code.messages.count, 1)
        compare(code.messages.get(0).text, "Code streamed reply")
        compare(code.messages.get(0).image, "file:///tmp/code.png")
        verify(!code.applyEvent({type: "msg", id: 42, text: "duplicate", out: false}))
        compare(code.messages.count, 1)
    }

    function test_background_send_ack_clears_only_its_chat() {
        ray.draft = "Keep Ray draft"
        ray.attachment = "file:///tmp/ray.png"
        code.draft = "Sent Code draft"
        code.attachment = "file:///tmp/code.png"
        code.sending = true
        code.applyEvent({type: "send-result", ok: true})
        compare(code.draft, "")
        compare(code.attachment, "")
        verify(!code.sending)
        compare(ray.draft, "Keep Ray draft")
        compare(ray.attachment, "file:///tmp/ray.png")
    }

    function test_failed_send_and_disconnect_preserve_drafts() {
        code.draft = "retry me"
        code.attachment = "file:///tmp/code.png"
        code.sending = true
        code.applyEvent({type: "send-result", ok: false, error: "offline"})
        verify(!code.sending)
        compare(code.sendError, "offline")
        compare(code.draft, "retry me")
        compare(code.attachment, "file:///tmp/code.png")
        code.sending = true
        code.disconnected()
        verify(!code.sending)
        verify(!code.ready)
        compare(code.draft, "retry me")
        compare(code.attachment, "file:///tmp/code.png")
    }

    function test_typing_expiry_is_independent() {
        ray.applyEvent({type: "typing", on: true})
        code.applyEvent({type: "typing", on: true})
        code.typingTimeout.interval = 10
        code.typingTimeout.restart()
        tryCompare(code, "typing", false)
        verify(ray.typing)
        ray.applyEvent({type: "msg", id: 43, text: "done", out: false})
        verify(!ray.typing)
    }

    function test_history_does_not_erase_live_streams_or_images() {
        code.applyEvent({type: "msg", id: 42, text: "live reply", out: false, hasImage: true})
        code.applyEvent({type: "image", id: 42, image: "file:///tmp/code.png"})
        code.applyEvent({type: "edit", id: 42, text: "latest reply", out: false, hasImage: true})
        code.applyEvent({type: "msg", id: 43, text: "newer reply", out: false})
        code.applyEvent({type: "history", messages: [{id: 41, text: "old"}, {id: 42, text: "stale"}]})
        compare(code.messages.count, 3)
        compare(code.messages.get(0).mid, 41)
        compare(code.messages.get(1).text, "latest reply")
        compare(code.messages.get(1).image, "file:///tmp/code.png")
        compare(code.messages.get(2).mid, 43)
        compare(ray.messages.count, 0)
    }

    function test_reconnect_refreshes_stale_messages_and_preserves_order() {
        code.applyEvent({type: "msg", id: 40, text: "older", out: false})
        code.applyEvent({type: "msg", id: 42, text: "before disconnect", out: false, hasImage: true})
        code.applyEvent({type: "image", id: 42, image: "file:///tmp/code.png"})
        code.disconnected()
        code.applyEvent({type: "history", messages: [{id: 42, text: "edited while offline", hasImage: true}, {id: 43, text: "new"}]})
        compare(code.messages.count, 3)
        compare(code.messages.get(0).mid, 40)
        compare(code.messages.get(1).text, "edited while offline")
        compare(code.messages.get(1).image, "file:///tmp/code.png")
        compare(code.messages.get(2).mid, 43)
    }
}
