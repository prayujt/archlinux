import importlib.machinery
import importlib.util
import io
import json
import tempfile
import unittest
import urllib.error
from pathlib import Path
from unittest.mock import patch


loader = importlib.machinery.SourceFileLoader(
    "notification_importance", str(Path(__file__).parents[1] / "notification-importance"))
spec = importlib.util.spec_from_loader(loader.name, loader)
importance = importlib.util.module_from_spec(spec)
loader.exec_module(importance)


class NotificationImportanceTests(unittest.TestCase):
    def test_reads_only_exact_entry_without_executing_shell(self):
        with tempfile.TemporaryDirectory() as directory:
            secrets = Path(directory) / "secrets"
            marker = Path(directory) / "executed"
            secrets.write_text(
                f"OTHER_KEY=$(touch {marker})\n"
                "TYPESAFE_API_KEY_BACKUP=wrong\n"
                "export TYPESAFE_API_KEY='test-key' # comment\n"
                "TYPESAFE_API_KEY=do-not-read-this-entry\n")
            self.assertEqual(importance.read_api_key(secrets), "test-key")
            self.assertFalse(marker.exists())

    def test_missing_or_malformed_key_does_not_return_other_values(self):
        with tempfile.TemporaryDirectory() as directory:
            secrets = Path(directory) / "secrets"
            for content in ("OTHER_KEY=private\n", "TYPESAFE_API_KEY=\n",
                            "TYPESAFE_API_KEY='unterminated\n"):
                secrets.write_text(content)
                with self.assertRaises(ValueError):
                    importance.read_api_key(secrets)

    def test_classifies_with_bounded_notification_data_only(self):
        for choice in ("normal", "important"):
            with self.subTest(choice=choice), patch.object(importance.urllib.request, "build_opener") as build:
                opener = build.return_value
                opener.open.return_value.__enter__.return_value = io.StringIO(json.dumps(
                    {"answers": {"importance": {"type": "choice", "choice": choice},
                                 "agent_attention": {"type": "choice", "choice": "none"}}}))
                notification = {"id": 7, "app": "Calendar", "summary": "Meeting in 2 minutes",
                                "body": "x" * 5000, "unrelated": "must stay local"}
                result = importance.evaluate(notification, "fake-key")
                self.assertEqual(result, {"id": 7, "important": choice == "important", "agentAttention": "none"})
                request = opener.open.call_args.args[0]
                self.assertEqual(request.full_url, importance.API_URL)
                self.assertEqual(request.get_header("Authorization"), "Bearer fake-key")
                payload = json.loads(request.data)
                self.assertEqual(set(payload["state"]), {"app", "summary", "body"})
                self.assertEqual(len(payload["state"]["body"]), 4000)
                self.assertEqual(set(payload["questions"]), {"importance"})
                self.assertEqual(opener.open.call_args.kwargs["timeout"], 4)

    def test_errors_keep_normal_preview_and_do_not_echo_secrets(self):
        failures = [TimeoutError("private-key"), ValueError("private-key"),
                    urllib.error.HTTPError(importance.API_URL, 401, "private-key", {}, None)]
        for failure in failures:
            with self.subTest(failure=failure), patch.object(importance, "classify", side_effect=failure):
                result = importance.evaluate({"id": 8}, "private-key")
                self.assertFalse(result["important"])
                self.assertIn("error", result)
                self.assertNotIn("private-key", json.dumps(result))
        self.assertEqual(importance.evaluate({"id": 9}, None),
                         {"id": 9, "important": False, "agentAttention": "none", "error": "Jev key unavailable"})

    def test_approval_and_required_input_always_promote_the_card(self):
        for attention in ("approval", "input"):
            with self.subTest(attention=attention), patch.object(importance.urllib.request, "build_opener") as build:
                build.return_value.open.return_value.__enter__.return_value = io.StringIO(json.dumps(
                    {"answers": {"importance": {"type": "choice", "choice": "normal"},
                                 "agent_attention": {"type": "choice", "choice": attention}}}))
                self.assertEqual(importance.evaluate({"id": 10, "app": "Ray Code"}, "fake-key"),
                                 {"id": 10, "important": True, "agentAttention": attention})
                payload = json.loads(build.return_value.open.call_args.args[0].data)
                self.assertEqual(set(payload["questions"]), {"importance", "agent_attention"})

    def test_invalid_agent_answer_does_not_create_a_persistent_card(self):
        with patch.object(importance.urllib.request, "build_opener") as build:
            build.return_value.open.return_value.__enter__.return_value = io.StringIO(json.dumps(
                {"answers": {"importance": {"type": "choice", "choice": "normal"},
                             "agent_attention": {"type": "choice", "choice": "unrecognized"}}}))
            result = importance.evaluate({"id": 11, "app": "Codex"}, "fake-key")
            self.assertFalse(result["important"])
            self.assertEqual(result["agentAttention"], "none")
            self.assertEqual(result["error"], "Jev unavailable")

    def test_slack_automation_cannot_become_a_persistent_agent_request(self):
        with patch.object(importance.urllib.request, "build_opener") as build:
            build.return_value.open.return_value.__enter__.return_value = io.StringIO(json.dumps(
                {"answers": {"importance": {"type": "choice", "choice": "normal"},
                             "agent_attention": {"type": "choice", "choice": "input"}}}))
            result = importance.evaluate({"id": 12, "app": "Slack", "summary":
                "[grid-polymarket] in resolutions-manual-sports-prod", "body":
                "Markets Automation Bot: @channel Manual settlement required - participant_mismatch"}, "fake-key")
            self.assertEqual(result, {"id": 12, "important": False, "agentAttention": "none"})
            payload = json.loads(build.return_value.open.call_args.args[0].data)
            self.assertNotIn("agent_attention", payload["questions"])

    def test_agent_tool_and_progress_updates_stay_small_without_a_model_call(self):
        messages = ["📚 Reading skill codex 💻 terminal date -u +%Y command -v uv",
                    "💻 terminal kubectl auth can-i patch deployments",
                    "⏳ Working – 1 min – codex app-server: item/completed",
                    "↪ Redirected current run. I'll adjust using your correction.",
                    "💡 First-time tip: send /busy queue to wait for a separate turn."]
        for app in ("Ray", "Ray Code"):
            for message in messages:
                with self.subTest(app=app, message=message), patch.object(importance.urllib.request, "build_opener") as build:
                    result = importance.evaluate({"id": 13, "app": app, "summary": message}, "fake-key")
                    self.assertEqual(result, {"id": 13, "important": False, "agentAttention": "none"})
                    build.assert_not_called()

    def test_redirect_cannot_forward_authorization(self):
        handler = importance.NoRedirect()
        self.assertIsNone(handler.redirect_request(None, None, 302, "", {}, "https://elsewhere.invalid"))


if __name__ == "__main__":
    unittest.main()
