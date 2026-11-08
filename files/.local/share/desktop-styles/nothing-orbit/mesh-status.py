#!/usr/bin/env python3
"""Expose local mesh status without forwarding authentication data or CLI errors."""
import json, subprocess, sys


def summarize(data):
    backend = data.get('BackendState', '')
    own = data.get('Self') or {}
    devices = []
    for peer in (data.get('Peer') or {}).values():
        name = peer.get('HostName') or peer.get('DNSName', '').split('.')[0] or 'Device'
        devices.append({'name': name, 'online': bool(peer.get('Online')), 'address': (peer.get('TailscaleIPs') or [''])[0]})
    devices.sort(key=lambda p: (not p['online'], p['name'].lower()))
    connected = backend == 'Running' and bool(own.get('Online'))
    return {'connected': connected, 'available': True,
            'state': 'Connected' if connected else 'Sign-in needed' if backend in ['NeedsLogin', 'NeedsMachineAuth'] else 'Disconnected',
            'canControl': backend in ['Running', 'Stopped'],
            'address': (own.get('TailscaleIPs') or [''])[0],
            'online': sum(p['online'] for p in devices), 'total': len(devices),
            'devices': devices, 'error': ''}


def status():
    try:
        result = subprocess.run(['/usr/bin/tailscale', 'status', '--json'], capture_output=True, text=True, timeout=5)
        if result.returncode:
            raise ValueError('Service unavailable')
        return summarize(json.loads(result.stdout))
    except (OSError, ValueError, subprocess.TimeoutExpired):
        return {'connected': False, 'available': False, 'state': 'Unavailable', 'canControl': False,
                'address': '', 'online': 0, 'total': 0, 'devices': [], 'error': 'Unable to read the mesh service.'}


if __name__ == '__main__':
    action = sys.argv[1] if len(sys.argv) > 1 else 'status'
    if action not in ['status', 'connect', 'disconnect']:
        sys.exit(2)
    error = ''
    if action != 'status':
        try:
            command = ['/usr/bin/tailscale', 'down'] if action == 'disconnect' else ['/usr/bin/tailscale', 'up', '--timeout=8s']
            result = subprocess.run(command, capture_output=True, text=True, timeout=10)
            if result.returncode:
                error = 'Unable to change the mesh connection.'
        except (OSError, subprocess.TimeoutExpired):
            error = 'The mesh connection update timed out.'
    data = status()
    data['error'] = error or data['error']
    print(json.dumps(data), flush=True)
