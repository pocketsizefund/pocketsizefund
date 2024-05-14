import unittest

from pkg.id import id


class TestGenerateId(unittest.TestCase):
    def test_new_id_success(self) -> None:
        result = id.new_id()

        self.assertIsInstance(
            result,
            str,
        )
