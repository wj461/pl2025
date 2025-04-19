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
    open(File, read, Stream),
    read_string(Stream, Text),
    close(Stream),

    filter_chars_and_normalize(Text, FilteredText),
    scan(FilteredText, WordList),
    remove_stop_words(WordList, StopWords, FilteredWordList),
    frequencies(FilteredWordList, WordFreq),
    sorted(WordFreq, SortedWordList),
    write(SortedWordList).

read_stop_words(File, StopWords) :-
    open(File, read, Stream),
    read_string(Stream, StopWordsString),
    close(Stream),
    split_string(StopWordsString, "\n", "", StopWordsList),
    maplist(string_lower, StopWordsList, StopWords).


filter_chars_and_normalize(Text, FilteredText) :-
    string_lower(Text, LowerText),
    string_codes(LowerText, Codes),
    exclude(is_not_alpha, Codes, FilteredCodes),
    string_codes(FilteredText, FilteredCodes).

is_not_alpha(Code) :-
    (Code < 97 ; Code > 122), % ASCII range for lowercase letters
    (Code < 65 ; Code > 90). % ASCII range for uppercase letters
    
scan(Text, WordList) :-
    split_string(Text, " ", "", Words),
    maplist(string_lower, Words, WordList).

remove_stop_words(WordList, StopWords, FilteredWordList) :-
    exclude(member(StopWords), WordList, FilteredWordList).

frequencies(WordList, WordFreq) :-
    findall(Word, member(Word, WordList), AllWords),
    sort(AllWords, UniqueWords),
    maplist(count_occurrences(AllWords), UniqueWords, WordFreq).

count_occurrences(AllWords, Word, Word-Count) :-
    include(=(Word), AllWords, FilteredWords),
    length(FilteredWords, Count).

sorted(WordFreq, SortedWordList) :-
    predsort(compare_word_freq, WordFreq, SortedWordList).

compare_word_freq(=, Word1-Count1, Word2-Count2) :-
    (Count1 > Count2 -> true ; Count1 < Count2 -> false ; Word1 @=< Word2).
