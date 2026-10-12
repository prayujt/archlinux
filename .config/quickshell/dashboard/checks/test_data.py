import importlib.util
import json
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch

spec = importlib.util.spec_from_file_location("dashboard_data", Path(__file__).parents[1] / "data.py")
dashboard = importlib.util.module_from_spec(spec)
spec.loader.exec_module(dashboard)


class DashboardDataTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.folder = Path(self.temporary.name)
        self.patches = patch.multiple(dashboard, STATE=self.folder / "state", CACHE=self.folder / "cache")
        self.patches.start()
        self.addCleanup(self.patches.stop)
        dashboard.action(["init"])

    def test_city_persists_and_startup_preserves_it(self):
        dashboard.action(["city", "Gainesville, FL"])
        dashboard.action(["init"])
        state = dashboard.preferences()
        self.assertEqual(state["city"], "Gainesville, FL")
        self.assertEqual((dashboard.STATE / "preferences.json").stat().st_mode & 0o777, 0o600)

    def test_sparklines_do_not_duplicate_cached_quotes(self):
        history = {}
        quote = {"symbol": "BTC-USD", "price": 100, "asOf": "2026-10-10T12:00:00Z"}
        dashboard.sample_quotes([quote.copy()], history)
        dashboard.sample_quotes([quote.copy()], history)
        self.assertEqual(len(history["BTC-USD"]), 1)
        rows = dashboard.sample_quotes([{**quote, "price": 101, "asOf": "2026-10-10T12:00:11Z"}], history)
        self.assertEqual(rows[0]["spark"], [100, 101])

    def test_bad_city_is_reported_without_destroying_cached_weather(self):
        cached = {"city": "New York, NY", "current": {"temperature_2m": 68}}
        dashboard.write_json(dashboard.CACHE / "weather.json", cached)
        with patch.object(dashboard, "fetch_json", return_value={"results": []}):
            with self.assertRaisesRegex(ValueError, "City not found"):
                dashboard.weather("Not a real city")
        self.assertEqual(json.loads((dashboard.CACHE / "weather.json").read_text()), cached)


if __name__ == "__main__":
    unittest.main()
