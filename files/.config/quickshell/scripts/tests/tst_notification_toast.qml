import QtQuick
import QtTest
import "../.."

TestCase {
    name: "NotificationAttention"
    NotificationToastState { id: toast }
    SignalSpy { id: requests; target: toast; signalName: "classify" }

    function init() {
        toast.expiry.stop()
        toast.debounce.stop()
        toast.shown = false
        toast.hovered = false
        toast.agentAttention = "none"
        toast.notifications = ({})
        toast.pending = ({})
        toast.waiting = []
        requests.clear()
    }
    function test_important_result_expands_and_extends_timeout() {
        toast.show("Calendar", "Meeting in two minutes", "Join the call", "calendar:1")
        verify(!toast.important)
        compare(toast.expiry.interval, 10000)
        toast.applyClassification({id: toast.serial, important: true})
        verify(toast.important)
        compare(toast.expiry.interval, 20000)
        compare(toast.body, "Join the call")
    }
    function test_late_results_cannot_promote_new_or_dismissed_toasts() {
        toast.show("Calendar", "Meeting", "", "1")
        let old = toast.serial
        toast.show("Music", "New track", "", "2")
        toast.applyClassification({id: old, important: true})
        verify(!toast.important)
        toast.shown = false
        toast.applyClassification({id: toast.serial, important: true})
        verify(!toast.important)
        verify(!toast.shown)
    }
    function test_stream_edits_are_debounced_and_scoped_to_the_message() {
        toast.show("Ray", "Starting", "", "ray:42")
        toast.update("ray:41", "Unrelated edit")
        compare(toast.text, "Starting")
        let old = toast.serial
        toast.update("ray:42", "Still working")
        toast.update("ray:42", "Done")
        toast.applyClassification({id: old, important: true})
        verify(!toast.important)
        requests.wait()
        compare(requests.count, 1)
        compare(requests.signalArguments[0][0].summary, "Done")
    }
    function test_failure_and_expiry_preserve_expected_behavior() {
        toast.show("Mail", "Routine update", "", "mail:1")
        toast.applyClassification({id: toast.serial, important: false, error: "Jev unavailable"})
        verify(!toast.important)
        verify(toast.shown)
        toast.hovered = true
        verify(!toast.expiry.running)
        toast.hovered = false
        verify(toast.expiry.running)
        toast.expiry.interval = 10
        toast.expiry.restart()
        tryCompare(toast, "shown", false)
        toast.applyClassification({id: toast.serial, important: true})
        verify(!toast.shown)
        verify(!toast.important)
        toast.expiry.interval = Qt.binding(() => toast.important ? 20000 : 10000)
    }
    function test_approval_stays_visible_through_hover_and_routine_notifications() {
        let approval = toast.show("Ray Code", "May I deploy this change?", "", "ray-code:1")
        toast.applyClassification({id: approval, important: false, agentAttention: "approval"})
        verify(toast.important)
        verify(toast.needsUser)
        verify(!toast.expiry.running)
        toast.hovered = true
        toast.hovered = false
        verify(!toast.expiry.running)
        let routine = toast.show("Music", "New track", "", "music:1")
        toast.applyClassification({id: routine, important: false, agentAttention: "none"})
        compare(toast.source, "ray-code:1")
        verify(toast.shown)
        toast.dismiss()
        verify(!toast.shown)
        toast.applyClassification({id: approval, important: true, agentAttention: "approval"})
        verify(!toast.shown)
    }
    function test_multiple_waiting_agents_are_shown_in_turn() {
        let ray = toast.show("Ray", "Which option should I use?", "", "ray:2")
        let code = toast.show("Ray Code", "Approve running the migration?", "", "ray-code:2")
        toast.applyClassification({id: ray, important: true, agentAttention: "input"})
        toast.applyClassification({id: code, important: true, agentAttention: "approval"})
        compare(toast.source, "ray:2")
        toast.dismiss()
        compare(toast.source, "ray-code:2")
        verify(toast.shown)
        verify(!toast.expiry.running)
        toast.dismiss()
        verify(!toast.shown)
    }
    function test_stream_can_request_approval_after_initial_preview_expires() {
        toast.show("Ray Code", "Working on it", "", "ray-code:3")
        toast.shown = false
        toast.update("ray-code:3", "I need your approval to continue. Proceed?")
        requests.wait()
        let request = requests.signalArguments[0][0]
        toast.applyClassification({id: request.id, important: true, agentAttention: "approval"})
        verify(toast.shown)
        verify(toast.needsUser)
        compare(toast.text, "I need your approval to continue. Proceed?")
    }
    function test_opening_or_replying_to_chat_clears_only_that_agents_requests() {
        let ray = toast.show("Ray", "Please choose a target", "", "ray:4")
        let code = toast.show("Ray Code", "May I proceed?", "", "ray-code:4")
        toast.applyClassification({id: ray, important: true, agentAttention: "input"})
        toast.applyClassification({id: code, important: true, agentAttention: "approval"})
        toast.resolveChat("ray")
        compare(toast.source, "ray-code:4")
        compare(toast.waiting.length, 1)
        toast.resolveChat("ray-code")
        verify(!toast.shown)
        toast.applyClassification({id: code, important: true, agentAttention: "approval"})
        verify(!toast.shown)
    }
    function test_stream_resolution_removes_waiting_card_and_stale_decision() {
        let old = toast.show("Ray", "May I proceed?", "", "ray:5")
        toast.applyClassification({id: old, important: true, agentAttention: "approval"})
        toast.update("ray:5", "Approval received. Continuing now.")
        let latest = toast.serial
        toast.applyClassification({id: old, important: true, agentAttention: "approval"})
        toast.applyClassification({id: latest, important: false, agentAttention: "none"})
        verify(!toast.needsUser)
        verify(!toast.shown)
        compare(toast.waiting.length, 0)
    }
}
