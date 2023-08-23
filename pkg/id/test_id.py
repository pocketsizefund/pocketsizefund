import unittest

from pkg.id import id


class TestGenerateId(unittest.TestCase):
    def test_generate_id_success(self):
        result = id.generate_id()

        self.assertIsInstance(
            result,
            str,
        )
