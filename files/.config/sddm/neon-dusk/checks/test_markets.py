import json
from datetime import datetime, timezone
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch

SOURCE = Path(__file__).parents[1] / "system/news-fetch"


class MarketTests(unittest.TestCase):
    def setUp(self):
        self.ns = {"__name__": "market_test"}
        exec(compile(SOURCE.read_text(), str(SOURCE), "exec"), self.ns)
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.cache = Path(self.temp.name)
        self.ns["CACHE"] = self.cache
        self.spec = ("^GSPC", "S&P 500", "index")

    def chart(self, days, closes, current_price=999, current_day="2026-10-09"):
        stamp = lambda day: int(datetime.fromisoformat(day + "T16:00:00+00:00").timestamp())
        return {"chart": {"result": [{"timestamp": [stamp(day) for day in days],
            "indicators": {"quote": [{"close": closes}]},
            "meta": {"regularMarketTime": stamp(current_day), "regularMarketPrice": current_price,
                     "chartPreviousClose": 50}}]}}

    def test_weekend_uses_friday_versus_thursday_not_month_start(self):
        chart = self.chart(["2026-10-07", "2026-10-08", "2026-10-09"], [90, 100, 110])
        for day in ("2026-10-10", "2026-10-11"):
            result = self.ns["index_quote"](chart, self.spec, datetime.fromisoformat(day + "T16:00:00+00:00"))
            self.assertEqual(result["price"], 110)
            self.assertEqual(result["change"], 10)
            self.assertEqual(result["percent"], 10)
            self.assertEqual(result["sessionDate"], "2026-10-09")

    def test_weekday_live_price_compares_to_previous_trading_close(self):
        chart = self.chart(["2026-10-08", "2026-10-09"], [100, 110], current_price=115)
        result = self.ns["index_quote"](chart, self.spec, datetime.fromisoformat("2026-10-09T17:00:00+00:00"))
        self.assertEqual(result["price"], 115)
        self.assertEqual(result["change"], 15)
        self.assertEqual(result["percent"], 15)

    def test_friday_holiday_keeps_actual_last_session_date(self):
        chart = self.chart(["2026-10-07", "2026-10-08"], [90, 100], current_day="2026-10-08")
        result = self.ns["index_quote"](chart, self.spec, datetime.fromisoformat("2026-10-11T16:00:00+00:00"))
        self.assertEqual(result["sessionDate"], "2026-10-08")
        self.assertEqual(result["change"], 10)

    def test_crypto_uses_24_hour_open(self):
        with patch.dict(self.ns, fetch=lambda *args: b'{"last":"210","open":"200"}'):
            result = self.ns["market_quote"](("BTC-USD", "Bitcoin", "crypto"), datetime.now(timezone.utc))
        self.assertEqual(result["change"], 10)
        self.assertEqual(result["percent"], 5)
        self.assertEqual(result["sessionDate"], "")

    def test_failed_quotes_preserve_values_and_original_timestamp(self):
        old = {"symbol": "BTC-USD", "price": 200, "asOf": "2026-10-10T10:00:00+00:00"}
        (self.cache / "markets.json").write_text(json.dumps({"items": [old]}))
        def unavailable(*args):
            raise OSError("offline")
        with patch.dict(self.ns, market_quote=unavailable):
            self.ns["refresh_markets"]()
        self.assertEqual(json.loads((self.cache / "markets.json").read_text())["items"], [old])

    def test_news_failure_does_not_erase_previous_edition(self):
        old = b'{"items":[{"title":"Saved edition"}]}'
        (self.cache / "news.json").write_bytes(old)
        def unavailable(*args):
            raise OSError("offline")
        with patch.dict(self.ns, refresh_markets=lambda: None, feed_data=unavailable):
            self.ns["main"]()
        self.assertEqual((self.cache / "news.json").read_bytes(), old)


if __name__ == "__main__":
    unittest.main()
