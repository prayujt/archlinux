#!/usr/bin/env python3
"""Build the seeded streets, plazas, skyline, and textures for the desktop world."""
import json
import math
from pathlib import Path
import random
import struct
import sys
import zlib

import numpy as np

seed = int(sys.argv[1]) if len(sys.argv) > 1 else 5070
rng = random.Random(seed)
root = Path(__file__).resolve().parent
width, depth, elevation, resolution = 10000, 9000, 100, 129
avenues = [-4050, -2700, -1200, 350, 1850, 3300, 4550]
streets = [-3700, -2350, -900, 550, 2100, 3550]

def road_distance(x, z):
    return min(min(abs(x - a) for a in avenues), min(abs(z - s) for s in streets), abs(x + z * .55 - 700) / math.hypot(1, .55))

def in_park(x, z):
    return -1050 < x < 100 and 850 < z < 1900

sectors = []
while len(sectors) < 20:
    x, z = rng.uniform(-3700, 3700), rng.uniform(-3200, 3200)
    if road_distance(x, z) < 330 or in_park(x, z):
        continue
    if any(math.hypot(x - s['x'], z - s['z']) < 1250 for s in sectors):
        continue
    sectors.append(dict(x=round(x, 2), z=round(z, 2), y=40, rotation=rng.choice([0, 90, 180, 270])))
rng.shuffle(sectors)
for i, s in enumerate(sectors):
    s['id'] = i + 1

# Urban ground stays level so walking and the close camera do not bob over hills.
heights = [102] * (resolution * resolution)
def ground(x, z):
    return 40

props = []
def occupied(x, z, w, d):
    return any(abs(x - s['x']) < w / 2 + 620 and abs(z - s['z']) < d / 2 + 620 for s in sectors)

def building(x, z, w, d, h, style, skyline=False):
    props.append(dict(kind='building', x=round(x, 2), y=40, z=round(z, 2), w=round(w * 1.2, 2), d=round(d * 1.2, 2), h=round(h * 1.8, 2),
                      style=style, tank=not skyline and rng.random() < .36, spire=skyline and h > 1900, skyline=skyline))

for ax, bx in zip(avenues, avenues[1:]):
    for az, bz in zip(streets, streets[1:]):
        for ix in range(2):
            for iz in range(2):
                x = ax + (bx - ax) * (.34 if ix == 0 else .66)
                z = az + (bz - az) * (.34 if iz == 0 else .66)
                w, d = rng.uniform(250, 340), rng.uniform(250, 340)
                if occupied(x, z, w, d) or road_distance(x, z) < max(w, d) / 2 + 220 or in_park(x, z):
                    continue
                h = rng.uniform(430, 950) if rng.random() < .76 else rng.uniform(1100, 1750)
                building(x, z, w, d, h, rng.randrange(6))

# A distant Manhattan-like silhouette, with setbacks, rooftop tanks, and spires.
for edge in range(4):
    for i in range(16):
        span = -5900 + i * 790 + rng.uniform(-150, 150)
        x, z = (span, -5100) if edge == 0 else (span, 5100) if edge == 1 else (-6100, span) if edge == 2 else (6100, span)
        building(x, z, rng.uniform(350, 700), rng.uniform(350, 650), rng.uniform(850, 3000), rng.randrange(6), True)

for a in avenues[1:-1]:
    for z in range(-3300, 3400, 680):
        x = a + 215
        if not occupied(x, z, 20, 20) and min(abs(z - s) for s in streets) > 270:
            props.append(dict(kind='lamp', x=x, y=40, z=z, rotation=90, radius=10))
for i in range(28):
    x, z = rng.uniform(-980, 30), rng.uniform(930, 1840)
    props.append(dict(kind='tree', x=round(x, 2), y=40, z=round(z, 2), scale=round(rng.uniform(.8, 1.3), 2), radius=18))

for s in streets[1:-1]:
    for x in [-3300, -1900, 1000, 2500]:
        if not occupied(x, s + 100, 190, 95) and min(abs(x - a) for a in avenues) > 350:
            props.append(dict(kind='car', x=x, y=40, z=s + 105, w=190, d=90, color=rng.choice(['#d3ad42', '#26333f', '#465965']), radius=100))

def png(path, size, rows, channels):
    def chunk(kind, data):
        return struct.pack('!I', len(data)) + kind + data + struct.pack('!I', zlib.crc32(kind + data) & 0xffffffff)
    data = b''.join(b'\0' + bytes(row) for row in rows)
    path.write_bytes(b'\x89PNG\r\n\x1a\n' + chunk(b'IHDR', struct.pack('!2I5B', size, size, 8, {1: 0, 3: 2, 4: 6}[channels], 0, 0, 0)) + chunk(b'IDAT', zlib.compress(data, 9)) + chunk(b'IEND', b''))

png(root / 'heightmap.png', resolution, [heights[i:i + resolution] for i in range(0, len(heights), resolution)], 1)
texture_size = 4096
axis_x = np.linspace(-width / 2, width / 2, texture_size, dtype=np.float32)
axis_z = np.linspace(-depth / 2, depth / 2, texture_size, dtype=np.float32)
x, z = axis_x[None, :], axis_z[:, None]
dx = np.min(np.abs(x - np.array(avenues)[:, None, None]), axis=0)
dz = np.min(np.abs(z - np.array(streets)[:, None, None]), axis=0)
distance = np.minimum(np.minimum(dx, dz), np.abs(x + z * .55 - 700) / math.hypot(1, .55))
color = np.empty((texture_size, texture_size, 3), dtype=np.int16)
color[:] = (64, 69, 76)
color[(x > -1050) & (x < 100) & (z > 850) & (z < 1900)] = (35, 65, 50)
sidewalk = (distance >= 190) & (distance < 280)
color[sidewalk] = (94, 99, 106)
seams = (np.mod(x, 90) < 1.5) | (np.mod(z, 90) < 1.5)
color[sidewalk & seams] = (75, 80, 87)
color[(distance >= 175) & (distance < 190)] = (154, 157, 160)
color[distance < 175] = (33, 39, 47)
lanes = ((dx < 2.5) & (dz > 270) & (np.mod(z, 110) < 68)) | ((dz < 2.5) & (dx > 270) & (np.mod(x, 110) < 68))
color[lanes] = (191, 160, 87)
zebra = ((dx > 210) & (dx < 270) & (dz < 145) & (np.mod(z, 28) < 15)) | ((dz > 210) & (dz < 270) & (dx < 145) & (np.mod(x, 28) < 15))
color[zebra] = (209, 211, 210)
texture_rng = np.random.default_rng(seed + 73)
color += texture_rng.integers(-1, 2, (texture_size, texture_size, 1), dtype=np.int16)
png(root / 'terrain-color.png', texture_size, np.clip(color, 0, 255).astype(np.uint8).reshape(texture_size, -1), 3)
del color, distance, dx, dz

# High resolution facade atlases stay cached while each building tiles them to its dimensions.
facade_dir = root / 'city'
facade_dir.mkdir(exist_ok=True)
y, x = np.indices((1024, 1024))
cx, cy = x % 256, y % 128
window = (cx > 64) & (cx < 192) & (cy > 22) & (cy < 108)
frame = window & ((cx < 70) | (cx > 186) | (cy < 28) | (cy > 102))
mullion = window & ((np.abs(cx - 128) < 2) | (np.abs(cy - 67) < 2))
sill = (cx > 57) & (cx < 199) & (cy >= 108) & (cy < 114)
for style in range(6):
    brick = np.array([(112, 79, 66), (140, 115, 93), (91, 103, 114), (61, 86, 110), (93, 83, 78), (118, 132, 145)][style])
    color = np.empty((1024, 1024, 3), dtype=np.int16)
    color[:] = brick
    mortar = (y % 3 == 0) | ((x + ((y // 3) % 2) * 16) % 32 == 0)
    color[mortar] -= 7 if style != 3 else 1
    tones = np.array([rng.choice([(193, 165, 118), (130, 151, 170), (49, 67, 86), (39, 56, 77)]) for _ in range(32)]).reshape(8, 4, 3)
    panes = tones[y // 128, x // 256].copy()
    reflection = ((192 - cx) / 128 * 8 + (108 - cy) / 86 * 5).astype(np.int16)
    panes += reflection[:, :, None]
    color[window] = panes[window]
    color[frame | mullion] = (44, 53, 64)
    color[sill] = np.minimum(255, brick + 30)
    color[cy == 125] = brick - 14
    color += texture_rng.integers(-1, 2, (1024, 1024, 1), dtype=np.int16)
    png(facade_dir / f'facade-{style}.png', 1024, np.clip(color, 0, 255).astype(np.uint8).reshape(1024, -1), 3)

shadow = []
for y in range(64):
    row = []
    for x in range(64):
        radius = math.hypot((x - 31.5) / 31.5, (y - 31.5) / 31.5)
        row.extend((8, 12, 18, round(max(0, 1 - radius) ** 1.5 * 115)))
    shadow.append(row)
png(root / 'avatar' / 'shadow.png', 64, shadow, 4)

js = '.pragma library\n'
for name, value in dict(seed=seed, width=width, depth=depth, elevation=elevation, resolution=resolution, sectors=sectors, props=props, heights=heights).items():
    js += 'var ' + name + ' = ' + json.dumps(value, separators=(',', ':')) + ';\n'
js += '\nfunction ground(x, z) { return 40; }\n'
(root / 'WorldMap.js').write_text(js)
print(f'City seed {seed}: {len(sectors)} scattered workspaces, {len(props)} buildings and street props')
