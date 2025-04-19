% import the read file moudule
:- use_module(library(readutil)).
:- use_module(library(pcre)).
% Write source code here

% word_frequencies(+File, +StopWordsFile)

% read_stop_words(+File, -StopWords)

% filter_chars_and_normalize(+Text, -FilteredText)

% scan(+Text, -WordList)

% remove_stop_words(+WordList, +StopWords, -FilteredWordList)

% frequencies(+WordList, -WordFreq)

% sorted(+WordList, -SortedWordList)

word_frequencies(File, StopWordsFile) :-
    read_stop_words(StopWordsFile, StopWords),
    read_file_to_string(File, String, []),
    filter_chars_and_normalize(String, FilteredText),
    scan(FilteredText, WordList),
    remove_stop_words(WordList, StopWords, FilteredWordList),
    frequencies(FilteredWordList, WordFreq),
    sorted(WordFreq, SortedWordList),
    top_25_words(SortedWordList, Top25),
    print_top25(Top25).

top_25_words(WordList, Top25) :-
    (   length(WordList, N), N =< 25
    ->  Top25 = WordList
    ;   length(Top25, 25),
        append(Top25, _, WordList)
    ).
    % print_top25(Top25).

% helper predicate to print Word-Count pairs
print_top25(WordList) :-
    forall(
        member(Word-Count, WordList),
        format("~w: ~d~n", [Word, Count])
    ).


read_stop_words(File, StopWords) :-
    read_file_to_string(File, String, []),
    split_string(String, ",", "\n\t", StopWordsList),
    maplist(string_lower, StopWordsList, StopWordsTemp),
    Alphabet = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m",
                "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"],
    append(StopWordsTemp, Alphabet, StopWords).

filter_chars_and_normalize(Text, FilteredText) :-
    string_lower(Text, LowerText),
    re_replace("[^a-z]"/g, " ", LowerText, FilteredText).

scan(Text, WordList) :-
    split_string(Text, " ", "", Words),
    exclude(=(""), Words, WordList).

remove_stop_words(WordList, StopWords, FilteredWordList) :-
    exclude({StopWords}/[Word]>>member(Word, StopWords), WordList, FilteredWordList).

frequencies(WordList, WordFreq) :-
    frequencies(WordList, [], WordFreq).

frequencies([], Acc, Acc).
frequencies([Word|Rest], Acc, WordFreq) :-
    atom_string(WordAtom, Word),
    (   select(WordAtom-Count, Acc, RestAcc) ->  NewCount is Count + 1,
        Acc1 = [WordAtom-NewCount|RestAcc];
        Acc1 = [WordAtom-1|Acc]
    ),
    frequencies(Rest, Acc1, WordFreq).


sorted(WordList, SortedWordList) :-
    predsort(compare_word_freq, WordList, SortedWordList).

compare_word_freq(Delta , Word1-Count1, Word2-Count2) :-
    compare(Delta, Count2, Count1).

