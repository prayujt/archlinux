.pragma library
.import "WorldMap.js" as Map

function initial(x, y, z) {
    return { x: x, y: y, z: z, vx: 0, vy: 0, vz: 0, grounded: true,
             anchor: null, ropeLength: 0, shot: 0 };
}

function buildingHit(origin, direction, limit, padding) {
    var nearest = limit, result = null;
    for (var i = 0; i < Map.props.length; i++) {
        var p = Map.props[i];
        if (p.kind !== "building") continue;
        var low = [p.x - p.w / 2 - padding, p.y - padding, p.z - p.d / 2 - padding];
        var high = [p.x + p.w / 2 + padding, p.y + p.h + 5 + padding, p.z + p.d / 2 + padding];
        var enter = 0, exit = nearest;
        for (var axis = 0; axis < 3; axis++) {
            if (Math.abs(direction[axis]) < 0.00001) {
                if (origin[axis] < low[axis] || origin[axis] > high[axis]) { exit = -1; break; }
            } else {
                var a = (low[axis] - origin[axis]) / direction[axis];
                var b = (high[axis] - origin[axis]) / direction[axis];
                enter = Math.max(enter, Math.min(a, b));
                exit = Math.min(exit, Math.max(a, b));
            }
        }
        if (exit >= enter && enter < nearest) {
            nearest = enter;
            result = { distance: enter, index: i, point: [origin[0] + direction[0] * enter,
                       origin[1] + direction[1] * enter, origin[2] + direction[2] * enter] };
        }
    }
    return result;
}

function blocked(x, y, z) {
    for (var i = 0; i < Map.props.length; i++) {
        var p = Map.props[i], dx = x - p.x, dz = z - p.z;
        if (p.kind === "building") {
            if (y < p.y + p.h + 7 && y + 175 > p.y
                && Math.abs(dx) < p.w / 2 + 24 && Math.abs(dz) < p.d / 2 + 24) return true;
        } else if (y < p.y + (p.kind === "car" ? 100 : 320)
                   && dx * dx + dz * dz < (p.radius + 24) * (p.radius + 24)) return true;
    }
    return false;
}

function floorAt(x, z, previousY) {
    var floor = Map.ground(x, z) + 2;
    for (var i = 0; i < Map.props.length; i++) {
        var p = Map.props[i], roof = p.y + p.h + 7;
        if (p.kind === "building" && previousY >= roof - 1
            && Math.abs(x - p.x) <= p.w / 2 + 5 && Math.abs(z - p.z) <= p.d / 2 + 5)
            floor = Math.max(floor, roof);
    }
    return floor;
}

function webTarget(state, yaw) {
    var angle = yaw * Math.PI / 180, best = null, bestScore = Infinity;
    var origin = [state.x, state.y + 135, state.z];
    for (var i = 0; i < Map.props.length; i++) {
        var p = Map.props[i];
        if (p.kind !== "building" || p.skyline) continue;
        var point = [Math.max(p.x - p.w / 2, Math.min(p.x + p.w / 2, state.x)),
                     Math.min(p.y + p.h - 25, Math.max(state.y + 650, p.y + p.h * 0.85)),
                     Math.max(p.z - p.d / 2, Math.min(p.z + p.d / 2, state.z))];
        var dx = point[0] - origin[0], dy = point[1] - origin[1], dz = point[2] - origin[2];
        var distance = Math.hypot(dx, dy, dz), horizontal = Math.max(1, Math.hypot(dx, dz));
        if (distance < 180 || distance > 3800) continue;
        var facing = (-Math.sin(angle) * dx - Math.cos(angle) * dz) / horizontal;
        if (facing < 0.1) continue;
        var score = distance * (1 + (1 - facing) * 0.6) * (dy < 80 ? 1.8 : 1);
        if (score >= bestScore) continue;
        var hit = buildingHit(origin, [dx / distance, dy / distance, dz / distance], distance + 10, 0);
        if (!hit || hit.distance < 100) continue;
        best = hit.point;
        bestScore = score;
    }
    return best;
}

function shoot(state, point, yaw) {
    if (!point) return state;
    var next = Object.assign({}, state);
    var dx = point[0] - state.x, dy = point[1] - state.y - 135, dz = point[2] - state.z;
    var distance = Math.hypot(dx, dy, dz);
    if (distance < 90 || distance > 4000) return state;
    next.anchor = point;
    next.ropeLength = distance;
    next.shot = 0;
    if (next.grounded) {
        var angle = yaw * Math.PI / 180;
        next.vx -= Math.sin(angle) * 260;
        next.vz -= Math.cos(angle) * 260;
        next.vy = 1350;
        next.grounded = false;
    }
    return next;
}

function release(state) {
    return Object.assign({}, state, { anchor: null, shot: 0 });
}

function jump(state) {
    if (!state.grounded && !state.anchor) return state;
    return Object.assign({}, state, { anchor: null, shot: 0, grounded: false,
                         vy: state.grounded ? 620 : Math.max(state.vy, 350) });
}

function step(state, controls, dt) {
    var s = Object.assign({}, state);
    var count = Math.ceil(dt * 120), h = dt / count;
    for (var tick = 0; tick < count; tick++) {
        var moving = controls.x !== 0 || controls.z !== 0;
        if (s.grounded && !s.anchor) {
            var speed = controls.run ? 650 : 200;
            var blend = 1 - Math.exp(-(moving ? 10 : 14) * h);
            s.vx += (controls.x * speed - s.vx) * blend;
            s.vz += (controls.z * speed - s.vz) * blend;
        } else {
            s.vx = (s.vx + controls.x * 950 * h) * Math.exp(-0.12 * h);
            s.vz = (s.vz + controls.z * 950 * h) * Math.exp(-0.12 * h);
        }
        var horizontal = Math.hypot(s.vx, s.vz);
        if (horizontal > 1800) { s.vx *= 1800 / horizontal; s.vz *= 1800 / horizontal; }
        s.vy -= 1150 * h;
        var x = s.x + s.vx * h, y = s.y + s.vy * h, z = s.z + s.vz * h;
        if (s.anchor) {
            s.shot = Math.min(1, s.shot + h * 7);
            if (s.shot >= 1) {
                var reel = controls.run ? 1200 : 900;
                s.ropeLength = Math.max(200, s.ropeLength - reel * h);
                var rx = x - s.anchor[0], ry = y + 135 - s.anchor[1], rz = z - s.anchor[2];
                var length = Math.hypot(rx, ry, rz);
                if (length > s.ropeLength) {
                    var nx = rx / length, ny = ry / length, nz = rz / length;
                    x = s.anchor[0] + nx * s.ropeLength;
                    y = s.anchor[1] + ny * s.ropeLength - 135;
                    z = s.anchor[2] + nz * s.ropeLength;
                    var outward = s.vx * nx + s.vy * ny + s.vz * nz;
                    if (outward > -reel) {
                        s.vx -= (outward + reel) * nx;
                        s.vy -= (outward + reel) * ny;
                        s.vz -= (outward + reel) * nz;
                    }
                }
            }
        }
        if (blocked(x, s.y, s.z)) { x = s.x; s.vx = 0; }
        if (blocked(x, s.y, z)) { z = s.z; s.vz = 0; }
        var boundedX = Math.max(-4700, Math.min(4700, x));
        var boundedZ = Math.max(-4200, Math.min(4200, z));
        if (x !== boundedX) s.vx = 0;
        if (z !== boundedZ) s.vz = 0;
        var floor = floorAt(boundedX, boundedZ, s.y);
        s.grounded = y <= floor && s.vy <= 0;
        if (s.grounded) { y = floor; s.vy = 0; }
        s.x = boundedX; s.y = y; s.z = boundedZ;
    }
    if (s.grounded && !controls.x && !controls.z && Math.hypot(s.vx, s.vz) < 0.5) { s.vx = 0; s.vz = 0; }
    if (s.anchor && s.shot >= 1) {
        var origin = [s.x, s.y + 135, s.z];
        var ray = [s.anchor[0] - origin[0], s.anchor[1] - origin[1], s.anchor[2] - origin[2]];
        var distance = Math.hypot(ray[0], ray[1], ray[2]);
        var hit = buildingHit(origin, ray.map(function(v) { return v / distance; }), distance, 0);
        if (hit && hit.distance < distance - 35) { s.anchor = null; s.shot = 0; }
    }
    return s;
}
