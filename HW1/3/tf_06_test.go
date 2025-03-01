package main

import (
	"reflect"
	"testing"
)

func TestReadFile(t *testing.T) {
	want := "This is a test."
	str := ReadFile("../test.txt")
	if str != want {
		t.Errorf("ReadFile() = %v, want %v", str, want)
	}
}

func TestFilterCharsAndNormalize(t *testing.T) {
	want := "this is a test "
	str := FilterCharsAndNormalize("This is a test.")
	if str != want {
		t.Errorf("FilterCharsAndNormalize() = %v, want %v", str, want)
	}
}

func TestScan(t *testing.T) {
	want := []string{"this", "is", "a", "test"}
	wordList := Scan("this is a test")
	if !reflect.DeepEqual(wordList, want) {
		t.Errorf("Scan() = %v, want %v", wordList, want)
	}
}

func TestRemoveStopWords(t *testing.T) {
	wordList := []string{"work", "is", "hard", "a", "work", "pays", "off"}
	want := []string{"work", "hard", "work", "pays"}
	newWordList := RemoveStopWords(wordList)("../../stop_words.txt")
	// ("../../stop_words.txt")
	if !reflect.DeepEqual(newWordList, want) {
		t.Errorf("RemoveStopWords() = %v, want %v", newWordList, want)
	}
}

func TestFrequencies(t *testing.T) {
	wordList := []string{"work", "work", "is", "hard", "a", "a", "work", "pays", "off"}
	want := map[string]int{"work": 3, "is": 1, "hard": 1, "a": 2, "pays": 1, "off": 1}
	wordFreqs := Frequencies(wordList)
	if !reflect.DeepEqual(wordFreqs, want) {
		t.Errorf("Frequencies() = %v, want %v", wordFreqs, want)
	}
}

func TestSort(t *testing.T) {
	wordFreqs := map[string]int{"work": 3, "is": 1, "hard": 1, "a": 2, "pays": 1, "off": 1}
	want := []pair{{"work", 3}, {"a", 2}, {"is", 1}, {"hard", 1}, {"pays", 1}, {"off", 1}}
	sortedWordFreqs := Sort(wordFreqs)
	if !reflect.DeepEqual(sortedWordFreqs, want) {
		t.Errorf("Sort() = %v, want %v", sortedWordFreqs, want)
	}
}
