#!/usr/bin/env python3
"""Local dashboard data. Public feeds come from the existing SDDM cache."""
import concurrent.futures
from datetime import datetime
import json
from pathlib import Path
import shutil
import subprocess
import sys
import time
from urllib.parse import urlencode
from urllib.request import Request, urlopen
from zoneinfo import ZoneInfo

STATE = Path.home() / ".local/state/quickshell-dashboard"
CACHE = Path.home() / ".cache/quickshell/dashboard"
FEEDS = Path("/var/cache/sddm-news")
TZ = ZoneInfo("America/New_York")


def read_json(path, default):
    try:
        return json.loads(path.read_text())
    except (OSError, ValueError):
        return default


def write_json(path, value):
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(".tmp")
    temporary.write_text(json.dumps(value, ensure_ascii=False))
    temporary.chmod(0o600)
    temporary.replace(path)


def preferences():
    return read_json(STATE / "preferences.json", {"city": "New York, NY"})


def action(arguments):
    state = preferences()
    operation = arguments[0]
    if operation == "init" and (STATE / "preferences.json").exists():
        return
    if operation == "city":
        city = arguments[1].strip()
        if not city:
            raise ValueError("Enter a city")
        state["city"] = city
    elif operation != "init":
        raise ValueError("Unknown preference operation")
    write_json(STATE / "preferences.json", state)


def fetch_json(url):
    with urlopen(Request(url, headers={"User-Agent": "NeonDuskDashboard/1.0"}), timeout=6) as response:
        return json.load(response)


def weather(city):
    cached = read_json(CACHE / "weather.json", {})
    location = cached.get("location") if cached.get("city") == city else None
    if not location:
        data = fetch_json("https://geocoding-api.open-meteo.com/v1/search?" +
                          urlencode({"name": city, "count": 1, "language": "en"}))
        if not data.get("results"):
            raise ValueError("City not found")
        location = data["results"][0]
    parameters = {"latitude": location["latitude"], "longitude": location["longitude"],
                  "current": "temperature_2m,apparent_temperature,weather_code,is_day",
                  "hourly": "temperature_2m,weather_code,precipitation_probability",
                  "temperature_unit": "fahrenheit", "timezone": "America/New_York",
                  "forecast_days": 2}
    data = fetch_json("https://api.open-meteo.com/v1/forecast?" + urlencode(parameters))
    now = datetime.now(TZ).replace(tzinfo=None)
    hours = []
    hourly = data["hourly"]
    for index, stamp in enumerate(hourly["time"]):
        if datetime.fromisoformat(stamp) >= now.replace(minute=0, second=0, microsecond=0):
            hours.append({"time": stamp, "temperature": hourly["temperature_2m"][index],
                          "code": hourly["weather_code"][index],
                          "rain": hourly["precipitation_probability"][index]})
    result = {"city": city, "location": location, "current": data["current"],
              "hours": hours[:6], "checked": datetime.now(TZ).isoformat(), "error": ""}
    write_json(CACHE / "weather.json", result)
    return result


class SystemStats:
    def __init__(self):
        self.cpu_previous = None
        self.network_previous = None
        self.gpu = {}
        self.gpu_checked = 0

    def snapshot(self):
        cpu = [int(value) for value in Path("/proc/stat").read_text().splitlines()[0].split()[1:9]]
        total, idle = sum(cpu), cpu[3] + cpu[4]
        usage = 0
        if self.cpu_previous:
            old_total, old_idle = self.cpu_previous
            usage = 100 * (1 - (idle - old_idle) / max(1, total - old_total))
        self.cpu_previous = total, idle
        memory = {}
        for line in Path("/proc/meminfo").read_text().splitlines():
            key, value, *_ = line.split()
            memory[key.rstrip(":")] = int(value)
        temperature = None
        for device in Path("/sys/class/hwmon").glob("hwmon*"):
            if (device / "name").read_text().strip() == "k10temp":
                temperature = float((device / "temp1_input").read_text()) / 1000
        if time.monotonic() - self.gpu_checked > 4:
            try:
                output = subprocess.check_output(["nvidia-smi", "--query-gpu=temperature.gpu,utilization.gpu,memory.used,memory.total",
                                                  "--format=csv,noheader,nounits"], text=True, timeout=1.5)
                temperature_gpu, load_gpu, used_gpu, total_gpu = map(float, output.splitlines()[0].split(","))
                self.gpu = {"temperature": temperature_gpu, "load": load_gpu,
                            "used": used_gpu, "total": total_gpu}
            except (OSError, ValueError, subprocess.SubprocessError):
                self.gpu = {}
            self.gpu_checked = time.monotonic()
        network, received, transmitted = "Offline", 0, 0
        for line in Path("/proc/net/route").read_text().splitlines()[1:]:
            fields = line.split()
            if fields[1] == "00000000" and int(fields[3], 16) & 1:
                network = fields[0]
                stats = Path("/sys/class/net") / network / "statistics"
                received = int((stats / "rx_bytes").read_text())
                transmitted = int((stats / "tx_bytes").read_text())
                break
        download, upload = 0, 0
        clock = time.monotonic()
        if self.network_previous and self.network_previous[0] == network:
            _, old_received, old_transmitted, old_clock = self.network_previous
            download = max(0, received - old_received) / (clock - old_clock)
            upload = max(0, transmitted - old_transmitted) / (clock - old_clock)
        self.network_previous = network, received, transmitted, clock
        disk = shutil.disk_usage(Path.home())
        return {"cpu": usage, "cpuTemperature": temperature, "gpu": self.gpu,
                "memoryUsed": (memory["MemTotal"] - memory["MemAvailable"]) / 1048576,
                "memoryTotal": memory["MemTotal"] / 1048576,
                "network": network, "download": download, "upload": upload,
                "diskFree": disk.free / 1073741824,
                "uptime": float(Path("/proc/uptime").read_text().split()[0])}


def sample_quotes(quotes, history):
    for row in quotes:
        points = history.setdefault(row["symbol"], [])
        if not points or points[-1]["time"] != row["asOf"]:
            points.append({"time": row["asOf"], "price": row["price"]})
            del points[:-90]
        row["spark"] = [point["price"] for point in points]
    return quotes


def stream():
    stats = SystemStats()
    history = read_json(CACHE / "market-history.json", {})
    forecast = read_json(CACHE / "weather.json", {})
    city, next_weather, job = forecast.get("city", ""), 0, None
    executor = concurrent.futures.ThreadPoolExecutor(max_workers=1)
    while True:
        state = preferences()
        if state["city"] != city:
            city, next_weather = state["city"], 0
            forecast = {"city": city, "error": "Loading forecast…"}
        if job and job.done():
            try:
                result = job.result()
                if result["city"] == city:
                    forecast = result
            except Exception as error:
                forecast = {**forecast, "error": "Forecast unavailable · check city or connection"}
                print("Weather:", type(error).__name__, file=sys.stderr)
            job = None
        if job is None and time.monotonic() >= next_weather:
            job = executor.submit(weather, city)
            next_weather = time.monotonic() + 600
        news = read_json(FEEDS / "news.json", {"items": []})
        quotes = sample_quotes(read_json(FEEDS / "markets.json", {"items": []})["items"], history)
        if int(time.monotonic()) % 10 < 2:
            write_json(CACHE / "market-history.json", history)
        print(json.dumps({"weather": forecast, "stats": stats.snapshot(),
                          "news": news,
                          "markets": quotes}), flush=True)
        time.sleep(2)


if __name__ == "__main__":
    try:
        if len(sys.argv) > 1:
            action(sys.argv[1:])
        else:
            stream()
    except (ValueError, IndexError) as error:
        print(str(error), file=sys.stderr)
        sys.exit(1)
    except BrokenPipeError:
        pass
