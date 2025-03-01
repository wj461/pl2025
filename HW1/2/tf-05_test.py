import unittest
# from tf_05 import filter_chars_and_normalize
import tf_05
import copy

class TestTF05(unittest.TestCase):

    def setUp(self):
        tf_05.data = []
        tf_05.words = []
        tf_05.word_freqs = []

    def test_read_file_not_idempotent(self):
        tf_05.read_file("../test.txt")
        first_data = tf_05.data
        tf_05.read_file("../test.txt")
        self.assertNotEqual(first_data, tf_05.data)

    def test_filter_chars_and_normalize_idempotent(self):
        sentence = "This is a test. This test is only a test."
        tf_05.data = list(sentence)
        tf_05.filter_chars_and_normalize()
        # deepcopy tf_05.data
        first_data = copy.deepcopy(tf_05.data)
        tf_05.filter_chars_and_normalize()
        self.assertEqual(first_data, tf_05.data)

    def test_scan_not_idempotent(self):
        sentence = "this is a test  this test is only a test "
        tf_05.data = list(sentence)
        tf_05.scan()
        first_words = copy.deepcopy(tf_05.words)
        tf_05.scan()
        self.assertNotEqual(first_words, tf_05.words)

    def test_remove_stop_words_idempotent(self):
        tf_05.words = ['test', 'test', 'test', 'a', 'this']
        tf_05.remove_stop_words()
        first_words = copy.deepcopy(tf_05.words)
        tf_05.remove_stop_words()
        self.assertEqual(first_words, tf_05.words)


    def test_frequencies_not_idempotent(self):
        tf_05.words = ['test', 'test', 'test']
        tf_05.frequencies()
        first_word_freqs = copy.deepcopy(tf_05.word_freqs)
        tf_05.frequencies()
        self.assertNotEqual(first_word_freqs, tf_05.word_freqs)

    def test_sort_idempotent(self):
        tf_05.word_freqs = [["test",1], ["notTest", 2]]
        tf_05.sort()
        first_word_freqs = copy.deepcopy(tf_05.word_freqs)
        tf_05.sort()
        self.assertEqual(first_word_freqs, tf_05.word_freqs)


if __name__ == '__main__':
    unittest.main()
