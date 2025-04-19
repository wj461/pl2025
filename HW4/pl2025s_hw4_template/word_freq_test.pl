:- use_module(library(plunit)).
:- use_module(word_freq).

:- begin_tests(word_freq_test).

test(read_stop_words) :-
    read_stop_words('./stop_words_test.txt', StopWords),
    assertion(StopWords == ["a", "an"]).

test(filter_chars_and_normalize) :-
    filter_chars_and_normalize("Hello, World!", FilteredText),
    assertion(FilteredText == "hello world").

test(scan) :-
    scan("hello world", WordList),
    assertion(WordList == ["hello", "world"]).

test(remove_stop_words) :-
    remove_stop_words(["hello", "world"], ["world"], FilteredWordList),
    assertion(FilteredWordList == ["hello"]).

test(frequencies) :-
    frequencies(["hello", "world", "hello"], WordFreq),
    assertion(WordFreq == [["hello", 2], ["world", 1]]).

test(sorted) :-
    sorted([only-1, test-2], SortedWordList),
    assertion(SortedWordList == [test-2, only-1]).

:- end_tests(word_freq_test).
