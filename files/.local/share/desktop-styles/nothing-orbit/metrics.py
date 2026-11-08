#!/usr/bin/env python3
import json, time
from pathlib import Path

def cpu_ticks():
    fields = [int(x) for x in Path('/proc/stat').read_text().splitlines()[0].split()[1:9]]
    return sum(fields), fields[3] + fields[4]

previous = cpu_ticks()
time.sleep(0.15)
while True:
    current = cpu_ticks()
    cpu = None
    if previous and current[0] > previous[0]:
        cpu = round(100 * (1 - (current[1] - previous[1]) / (current[0] - previous[0])))
    previous = current
    memory = {line.split(':')[0]: int(line.split()[1]) for line in Path('/proc/meminfo').read_text().splitlines()}
    used = memory['MemTotal'] - memory['MemAvailable']
    minutes = int(float(Path('/proc/uptime').read_text().split()[0])) // 60
    uptime = f'{minutes // 1440:02d}D {minutes % 1440 // 60:02d}H' if minutes >= 1440 else f'{minutes // 60:02d}H {minutes % 60:02d}M'
    print(json.dumps({'cpu': cpu, 'memory': round(100 * used / memory['MemTotal']), 'memoryGiB': round(used / 1024**2, 1), 'memoryTotalGiB': round(memory['MemTotal'] / 1024**2, 1), 'uptime': uptime}), flush=True)
    time.sleep(3)
