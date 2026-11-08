import asyncio
import importlib.machinery
import importlib.util
import json
import os
from pathlib import Path
import subprocess
import tempfile
from types import SimpleNamespace
import unittest
from unittest.mock import AsyncMock, patch


def load_script(name):
    loader = importlib.machinery.SourceFileLoader(name, str(Path(__file__).parents[1] / name))
    module = importlib.util.module_from_spec(importlib.util.spec_from_loader(name, loader))
    loader.exec_module(module)
    return module


bridge = load_script("telegram-ray")
clipboard = load_script("telegram-image")


def message(photo=True):
    return SimpleNamespace(id=42, chat_id=123, out=True, message="caption",
                           photo=SimpleNamespace(id=99) if photo else None,
                           document=None, file=SimpleNamespace(ext=".png"))


class TelegramImages(unittest.IsolatedAsyncioTestCase):
    async def test_send_routes_to_selected_bot_and_tags_all_results(self):
        client = SimpleNamespace(send_message=AsyncMock(return_value=message(False)))
        entities = {"ray": "ray-peer", "ray-code": "code-peer"}
        for chat, peer in entities.items():
            with self.subTest(chat=chat), patch.object(bridge, "emit") as emit:
                await bridge.dispatch_send(client, entities, json.dumps({"type": "send", "chat": chat, "text": "draft"}))
                self.assertEqual(client.send_message.call_args.args, (peer, "draft"))
                self.assertEqual([call.args[0]["chat"] for call in emit.call_args_list], [chat, chat])
                self.assertTrue(emit.call_args.args[0]["ok"])

    async def test_missing_chat_never_falls_back_to_other_bot(self):
        client = SimpleNamespace(send_message=AsyncMock())
        with patch.object(bridge, "emit") as emit:
            await bridge.dispatch_send(client, {"ray": "ray-peer"}, json.dumps({"type": "send", "chat": "ray-code", "text": "private"}))
        client.send_message.assert_not_awaited()
        self.assertEqual(emit.call_args.args[0]["chat"], "ray-code")
        self.assertFalse(emit.call_args.args[0]["ok"])

    async def test_background_send_failure_keeps_original_chat(self):
        client = SimpleNamespace(send_message=AsyncMock(side_effect=RuntimeError("offline")))
        with patch.object(bridge, "emit") as emit:
            await bridge.dispatch_send(client, {"ray-code": "code-peer"}, json.dumps({"type": "send", "chat": "ray-code", "text": "draft"}))
        self.assertEqual(emit.call_args.args[0]["chat"], "ray-code")
        self.assertIn("offline", emit.call_args.args[0]["error"])

    async def test_image_result_keeps_chat_identity(self):
        async def download(m, file):
            file.write(b"image")
        client = SimpleNamespace(download_media=AsyncMock(side_effect=download))
        with tempfile.TemporaryDirectory() as directory, patch.object(bridge, "MEDIA_DIR", Path(directory)), patch.object(bridge, "emit") as emit:
            await bridge.load_image(client, message(), "ray-code")
        self.assertEqual(emit.call_args.args[0]["chat"], "ray-code")

    async def test_shared_connection_routes_history_live_edits_and_typing(self):
        peers = {"ray": SimpleNamespace(id=101), "code": SimpleNamespace(id=202)}
        handlers = []

        def register(event):
            def decorate(handler):
                handlers.append((event, handler))
                return handler
            return decorate

        async def history(peer, limit):
            m = message(False)
            m.chat_id = peer.id
            yield m

        async def connected():
            await asyncio.sleep(0)
            for event, handler in handlers:
                if event is bridge.events.UserUpdate:
                    await handler(SimpleNamespace(user_id=202, typing=True))
                    await handler(SimpleNamespace(user_id=999, typing=True))
                else:
                    m = message(False)
                    m.chat_id = 202
                    await handler(SimpleNamespace(chat_id=202, message=m))

        client = SimpleNamespace(connect=AsyncMock(), disconnect=AsyncMock(),
                                 is_user_authorized=AsyncMock(return_value=True),
                                 get_entity=AsyncMock(side_effect=lambda key: peers[key]),
                                 iter_messages=history, on=register,
                                 run_until_disconnected=connected)
        with patch.object(bridge, "load_env", return_value={"API_ID": "1", "API_HASH": "test", "TG_RAY": "ray", "TG_RAY_CODE": "code"}), \
             patch.object(bridge, "TelegramClient", return_value=client) as factory, \
             patch.object(bridge.threading, "Thread"), patch.object(bridge, "emit") as emit:
            await bridge.run()
        factory.assert_called_once()
        client.disconnect.assert_awaited_once()
        events = [call.args[0] for call in emit.call_args_list]
        self.assertEqual({e["chat"] for e in events if e["type"] == "history"}, {"ray", "ray-code"})
        self.assertEqual({e["chat"] for e in events if e["type"] in {"msg", "edit", "typing"}}, {"ray-code"})
        self.assertEqual(sum(e["type"] == "typing" and e.get("on") is True for e in events), 1)

    async def test_stalled_connection_times_out_and_disconnects(self):
        async def stalled_connect():
            await asyncio.Event().wait()
        client = SimpleNamespace(connect=AsyncMock(side_effect=stalled_connect), disconnect=AsyncMock())
        deadline = asyncio.timeout(0.01)
        with patch.object(bridge, "load_env", return_value={"API_ID": "1", "API_HASH": "test", "TG_RAY": "ray"}), \
             patch.object(bridge, "TelegramClient", return_value=client), \
             patch.object(bridge.asyncio, "timeout", return_value=deadline) as timeout, \
             patch.object(bridge, "emit") as emit:
            with self.assertRaises(TimeoutError):
                await bridge.run()
        timeout.assert_called_once_with(30)
        client.disconnect.assert_awaited_once()
        self.assertFalse(emit.call_args.args[0]["ok"])
        self.assertIn("Connecting", emit.call_args.args[0]["error"])

    async def test_unauthorized_session_disconnects_with_clear_error(self):
        client = SimpleNamespace(connect=AsyncMock(), is_user_authorized=AsyncMock(return_value=False),
                                 disconnect=AsyncMock())
        with patch.object(bridge, "load_env", return_value={"API_ID": "1", "API_HASH": "test", "TG_RAY": "ray"}), \
             patch.object(bridge, "TelegramClient", return_value=client), patch.object(bridge, "emit"):
            with self.assertRaisesRegex(ValueError, "not authorized"):
                await bridge.run()
        client.disconnect.assert_awaited_once()

    def test_multiline_and_literal_json_text(self):
        text = 'Caption\nwith "quotes" and $(literal shell syntax)'
        self.assertEqual(bridge.parse_send(json.dumps({"type": "send", "text": text})), (text, ""))
        self.assertEqual(bridge.parse_send('{"hello": "world"}'), ('{"hello": "world"}', ""))

    async def test_photo_caption_and_success_ack(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "image with spaces.png"
            path.write_bytes(b"image fixture")
            client = SimpleNamespace(send_file=AsyncMock(return_value=message()), send_message=AsyncMock())
            request = json.dumps({"type": "send", "text": "caption\nsecond line", "image": path.as_uri()})
            with patch.object(bridge, "emit") as emit:
                await bridge.send_message(client, "ray", request)
            client.send_file.assert_awaited_once_with("ray", str(path), caption="caption\nsecond line",
                                                    parse_mode=None, force_document=False)
            client.send_message.assert_not_awaited()
            self.assertEqual(emit.call_args_list[0].args[0]["image"], path.as_uri())
            self.assertTrue(emit.call_args_list[-1].args[0]["ok"])

    async def test_image_only_and_failed_upload(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "image.webp"
            path.write_bytes(b"image fixture")
            client = SimpleNamespace(send_file=AsyncMock(side_effect=RuntimeError("upload failed")))
            with patch.object(bridge, "emit") as emit:
                await bridge.send_message(client, "ray", json.dumps({"type": "send", "text": "", "image": path.as_uri()}))
            self.assertEqual(client.send_file.call_args.kwargs["caption"], "")
            self.assertTrue(client.send_file.call_args.kwargs["force_document"])
            emit.assert_called_once()
            self.assertEqual(emit.call_args.args[0]["type"], "send-result")
            self.assertFalse(emit.call_args.args[0]["ok"])

    async def test_remote_image_rejected_before_send(self):
        client = SimpleNamespace(send_file=AsyncMock())
        with patch.object(bridge, "emit") as emit:
            await bridge.send_message(client, "ray", json.dumps({"type": "send", "image": "https://example.com/photo.png"}))
        client.send_file.assert_not_awaited()
        self.assertFalse(emit.call_args.args[0]["ok"])

    async def test_text_send_unchanged(self):
        client = SimpleNamespace(send_message=AsyncMock(return_value=message(False)))
        with patch.object(bridge, "emit"):
            await bridge.send_message(client, "ray", json.dumps({"type": "send", "text": "**literal**\nnext line"}))
        client.send_message.assert_awaited_once_with("ray", "**literal**\nnext line")

    async def test_download_is_cached_and_private(self):
        async def download(m, file):
            file.write(b"downloaded image")
        client = SimpleNamespace(download_media=AsyncMock(side_effect=download))
        with tempfile.TemporaryDirectory() as directory, patch.object(bridge, "MEDIA_DIR", Path(directory)), patch.object(bridge, "emit") as emit:
            await bridge.load_image(client, message())
            await bridge.load_image(client, message())
            client.download_media.assert_awaited_once()
            image = Path(directory) / "123_42_99.png"
            self.assertEqual(image.read_bytes(), b"downloaded image")
            self.assertEqual(image.stat().st_mode & 0o777, 0o600)
            self.assertEqual(emit.call_args.args[0]["image"], image.as_uri())

    async def test_failed_download_does_not_leave_partial_file(self):
        client = SimpleNamespace(download_media=AsyncMock(side_effect=OSError("network down")))
        with tempfile.TemporaryDirectory() as directory, patch.object(bridge, "MEDIA_DIR", Path(directory)), patch.object(bridge, "emit") as emit:
            await bridge.load_image(client, message())
            self.assertEqual(list(Path(directory).iterdir()), [])
            self.assertIn("network down", emit.call_args.args[0]["error"])

    def test_image_documents_and_text_messages(self):
        m = message(False)
        self.assertFalse(bridge.message_payload(m)["hasImage"])
        m.document = SimpleNamespace(id=8, mime_type="image/webp")
        self.assertTrue(bridge.message_payload(m)["hasImage"])
        m.document.mime_type = "application/pdf"
        self.assertFalse(bridge.message_payload(m)["hasImage"])


class ClipboardImages(unittest.TestCase):
    def test_picker_preserves_path_and_handles_cancel(self):
        path = Path("/tmp/image with spaces.png")
        with patch.object(clipboard.subprocess, "run", return_value=subprocess.CompletedProcess([], 0, os.fsencode(path) + b"\n")):
            self.assertEqual(clipboard.pick_image(), {"image": path.as_uri()})
        with patch.object(clipboard.subprocess, "run", return_value=subprocess.CompletedProcess([], 1, b"")):
            self.assertEqual(clipboard.pick_image(), {"image": ""})

    def test_binary_image_takes_priority_over_text(self):
        def run(args, **kwargs):
            if "--list-types" in args:
                return subprocess.CompletedProcess(args, 0, b"text/plain\nimage/png\n")
            kwargs["stdout"].write(b"\x89PNG\x00\r\nfixture")
            return subprocess.CompletedProcess(args, 0)
        with tempfile.TemporaryDirectory() as directory, patch.dict(clipboard.os.environ, {"XDG_CACHE_HOME": directory}), patch.object(clipboard.subprocess, "run", side_effect=run):
            result = clipboard.paste_image()
            path = Path(clipboard.unquote(clipboard.urlsplit(result["image"]).path))
            self.assertEqual(path.read_bytes(), b"\x89PNG\x00\r\nfixture")
            self.assertEqual(path.stat().st_mode & 0o777, 0o600)

    def test_copied_image_file(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "a picture.jpeg"
            path.touch()
            responses = [subprocess.CompletedProcess([], 0, b"text/uri-list\n"),
                         subprocess.CompletedProcess([], 0, (path.as_uri() + "\r\n").encode())]
            with patch.object(clipboard.subprocess, "run", side_effect=responses):
                self.assertEqual(clipboard.paste_image(), {"image": path.as_uri()})

    def test_text_is_captured_for_paste_after_switching_chats(self):
        responses = [subprocess.CompletedProcess([], 0, b"text/plain\n"),
                     subprocess.CompletedProcess([], 0, b"pasted text\nsecond line")]
        with patch.object(clipboard.subprocess, "run", side_effect=responses):
            self.assertEqual(clipboard.paste_image(), {"image": "", "text": "pasted text\nsecond line"})

    def test_empty_clipboard_uses_native_paste(self):
        with patch.object(clipboard.subprocess, "run", return_value=subprocess.CompletedProcess([], 1, b"")):
            self.assertEqual(clipboard.paste_image(), {"image": ""})


if __name__ == "__main__":
    unittest.main()
