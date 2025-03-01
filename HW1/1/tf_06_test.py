import unittest
import tf_06

class TestTf06(unittest.TestCase):
    def test_read_file(self):
        self.assertEqual(tf_06.read_file('../test.txt'), 'This is a test.')

    def test_filter_chars_and_normalize(self):
        self.assertEqual(tf_06.filter_chars_and_normalize('This is a test.'), 'this is a test ')

    def test_scan(self):
        self.assertEqual(tf_06.scan('this is a test '), ['this', 'is', 'a', 'test'])

    def test_remove_stop_words(self):
        word_list = ['work', 'is', 'hard', 'a', 'work', 'pays', 'off']
        result = tf_06.remove_stop_words(word_list)('../../stop_words.txt')
        self.assertEqual(result, ['work', 'hard', 'work', 'pays'])

    def test_frequencies(self):
        word_list = ['work', 'hard', 'work', 'pays']
        self.assertEqual(tf_06.frequencies(word_list), {'work': 2, 'hard': 1, 'pays': 1})

    def test_sort(self):
        word_freq = {'work': 3, 'hard': 1, 'pays': 2}
        self.assertEqual(tf_06.sort(word_freq), [('work', 3), ('pays', 2), ('hard', 1)])

if __name__ == '__main__':
    unittest.main()
