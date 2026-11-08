import importlib.machinery
import importlib.util
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch


loader = importlib.machinery.SourceFileLoader('sonos_info', str(Path(__file__).parents[1] / 'sonos-info'))
spec = importlib.util.spec_from_loader(loader.name, loader)
sonos = importlib.util.module_from_spec(spec)
loader.exec_module(sonos)

METADATA = '''<DIDL-Lite xmlns="urn:schemas-upnp-org:metadata-1-0/DIDL-Lite/"
    xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:upnp="urn:schemas-upnp-org:metadata-1-0/upnp/">
    <item><dc:title>Living Proof</dc:title><upnp:artist>Gregory Alan Isakov</upnp:artist>
    <upnp:album>Gregory Alan Isakov with the Colorado Symphony</upnp:album>
    <res protocolInfo="http-get:*:audio/mpeg:*">https://example.invalid/audio.mp3</res></item>
</DIDL-Lite>'''


class SonosInfoTests(unittest.TestCase):
    def test_recovers_direct_stream_artist_artwork_and_duration(self):
        info = {'title': 'Living Proof', 'artist': '', 'album_art': '',
                'duration': '0:00:00', 'position': '0:00:28', 'metadata': METADATA}
        result = sonos.snapshot(info, 'PLAYING', 25, False,
                               {'duration': 247, 'albumArt': 'file:///cache/cover.jpg'})
        self.assertEqual(result['artist'], 'Gregory Alan Isakov')
        self.assertEqual(result['album'], 'Gregory Alan Isakov with the Colorado Symphony')
        self.assertEqual(result['duration'], 247)
        self.assertEqual(result['position'], 28)
        self.assertEqual(result['albumArt'], 'file:///cache/cover.jpg')

    def test_native_speaker_metadata_takes_precedence(self):
        info = {'title': 'Native title', 'artist': 'Native artist', 'album': 'Native album',
                'duration': '0:03:42', 'position': '0:01:03', 'album_art': '/cover.jpg'}
        result = sonos.snapshot(info, 'PAUSED_PLAYBACK', 10, True,
                               {'duration': 999})
        self.assertEqual(result['duration'], 222)
        self.assertEqual(result['artist'], 'Native artist')
        self.assertEqual(result['albumArt'], 'http://192.168.1.50:1400/cover.jpg')
        self.assertEqual(result['state'], 'PAUSED_PLAYBACK')
        self.assertTrue(result['muted'])

    def test_embedded_art_replaces_a_sonos_proxy_url_that_can_return_404(self):
        result = sonos.snapshot({'album_art': '/getaa?u=unavailable-proxy'}, 'PLAYING', 10, False,
                               {'albumArt': 'file:///cache/cover.jpg'})
        self.assertEqual(result['albumArt'], 'file:///cache/cover.jpg')

    def test_reads_duration_and_art_from_didl_when_available(self):
        metadata = METADATA.replace('<res protocolInfo=',
            '<upnp:albumArtURI>/album.jpg</upnp:albumArtURI><res duration="0:04:07.8" protocolInfo=')
        result = sonos.snapshot({'metadata': metadata}, 'PLAYING', 15, False)
        self.assertEqual(result['duration'], 247)
        self.assertEqual(result['albumArt'], 'http://192.168.1.50:1400/album.jpg')

    def test_unknown_and_fractional_times(self):
        for value, expected in (('', 0), (None, 0), ('NOT_IMPLEMENTED', 0),
                                ('0:04:07.5', 247), ('1:02:03', 3723)):
            self.assertEqual(sonos.to_secs(value), expected)

    def test_embedded_metadata_is_cached_per_stream_and_never_reused_for_another_track(self):
        with tempfile.TemporaryDirectory() as directory, patch.dict(sonos.os.environ, {'XDG_CACHE_HOME': directory}), \
                patch.object(sonos, 'probe_stream', side_effect=[{'duration': 247}, {'duration': 180}]) as probe:
            resolver = sonos.StreamMetadata()
            self.assertEqual(resolver.get('https://example.invalid/one')['duration'], 247)
            self.assertEqual(resolver.get('https://example.invalid/one')['duration'], 247)
            self.assertEqual(probe.call_count, 1)
            self.assertEqual(resolver.get('https://example.invalid/two')['duration'], 180)
            self.assertEqual(probe.call_count, 2)
            self.assertEqual(resolver.get('x-sonos-spotify:track'), {})

    def test_failed_probe_is_rate_limited_and_keeps_speaker_position(self):
        with tempfile.TemporaryDirectory() as directory, patch.dict(sonos.os.environ, {'XDG_CACHE_HOME': directory}), \
                patch.object(sonos, 'probe_stream', side_effect=OSError('private playback URL')) as probe:
            resolver = sonos.StreamMetadata()
            self.assertEqual(resolver.get('https://example.invalid/private'), {})
            self.assertEqual(resolver.get('https://example.invalid/private'), {})
            self.assertEqual(probe.call_count, 1)
            result = sonos.snapshot({'position': '0:00:42'}, 'PLAYING', 10, False, resolver.fields)
            self.assertEqual(result['position'], 42)
            self.assertEqual(result['duration'], 0)


if __name__ == '__main__':
    unittest.main()
