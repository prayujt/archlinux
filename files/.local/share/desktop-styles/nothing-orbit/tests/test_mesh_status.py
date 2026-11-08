import contextlib
import importlib.util
import io
import json
from pathlib import Path
import runpy
import subprocess
import sys
import unittest
from unittest.mock import patch

script = Path(__file__).parents[1] / 'mesh-status.py'
spec = importlib.util.spec_from_file_location('mesh_status', script)
mesh = importlib.util.module_from_spec(spec)
spec.loader.exec_module(mesh)


def sample():
    return {'BackendState': 'Running', 'Self': {'Online': True, 'TailscaleIPs': ['100.64.0.1']},
            'AuthURL': 'PRIVATE_SIGN_IN_LINK', 'User': {'1': {'LoginName': 'private@example.invalid'}},
            'Peer': {'a': {'HostName': 'sleeping', 'Online': False},
                     'b': {'HostName': 'online', 'Online': True, 'TailscaleIPs': ['100.64.0.2']}}}


class MeshStatusTests(unittest.TestCase):
    def test_connected_state_counts_and_sorts_live_devices(self):
        value = mesh.summarize(sample())
        self.assertTrue(value['connected'])
        self.assertEqual((value['online'], value['total']), (1, 2))
        self.assertEqual(value['devices'][0]['name'], 'online')

    def test_status_does_not_forward_authentication_or_account_data(self):
        value = json.dumps(mesh.summarize(sample()))
        self.assertNotIn('PRIVATE_SIGN_IN_LINK', value)
        self.assertNotIn('private@example.invalid', value)
        self.assertNotIn('AuthURL', value)

    def test_offline_machine_does_not_claim_to_be_connected(self):
        value = sample()
        value['Self']['Online'] = False
        self.assertFalse(mesh.summarize(value)['connected'])

    def test_authentication_required_disables_connection_actions(self):
        value = mesh.summarize({'BackendState': 'NeedsLogin'})
        self.assertFalse(value['connected'])
        self.assertFalse(value['canControl'])
        self.assertEqual(value['state'], 'Sign-in needed')

    def test_missing_service_has_generic_error(self):
        with patch.object(mesh.subprocess, 'run', side_effect=OSError('PRIVATE_SIGN_IN_LINK')):
            value = mesh.status()
        self.assertFalse(value['available'])
        self.assertNotIn('PRIVATE_SIGN_IN_LINK', json.dumps(value))

    def test_action_failure_never_forwards_cli_output(self):
        failure = subprocess.CompletedProcess([], 1, 'PRIVATE_SIGN_IN_LINK', 'private@example.invalid')
        healthy = subprocess.CompletedProcess([], 0, json.dumps(sample()), '')
        output = io.StringIO()
        with patch.object(sys, 'argv', [str(script), 'connect']), patch('subprocess.run', side_effect=[failure, healthy]), contextlib.redirect_stdout(output):
            runpy.run_path(str(script), run_name='__main__')
        value = json.loads(output.getvalue())
        self.assertEqual(value['error'], 'Unable to change the mesh connection.')
        self.assertNotIn('PRIVATE_SIGN_IN_LINK', output.getvalue())
        self.assertNotIn('private@example.invalid', output.getvalue())


if __name__ == '__main__':
    unittest.main()
