package main

import (
	"bytes"
	"os"
	"reflect"
	"testing"

	"github.com/google/go-cmp/cmp"
)

func TestDataStorageManagerNewDataStorageManager(t *testing.T) {
	want := "this is a test "
	dsm := DataStorageManager{}
	dsm.NewDataStorageManager("test.txt")
	if dsm.strData != want {
		t.Errorf("NewDataStorageManager() = %v, want %v", dsm.strData, want)
	}
}

func TestDataStorageManagerWords(t *testing.T) {
	want := []string{"this", "is", "a", "test"}
	dsm := DataStorageManager{}
	dsm.NewDataStorageManager("test.txt")
	wordList := dsm.Words()
	if !reflect.DeepEqual(wordList, want) {
		t.Errorf("Words() = %v, want %v", wordList, want)
	}
}

func TestStopWordManagerIsStopWord(t *testing.T) {
	want := true
	swm := StopWordManager{}
	swm.NewStopWordManager()
	isStopWord := swm.IsStopWord("a")
	if isStopWord != want {
		t.Errorf("IsStopWord() = %v, want %v", isStopWord, want)
	}
}

func TestWordFrequencyManagerIncrementCount(t *testing.T) {
	want := 1
	wfm := WordFrequencyManager{}
	wfm.NewWordFrequencyManager()
	wfm.IncrementCount("test")
	if wfm.wordFreqs["test"] != want {
		t.Errorf("IncrementCount() = %v, want %v", wfm.wordFreqs["test"], want)
	}
}

func TestWordFrequencyManagerSort(t *testing.T) {
	want := []pair{{"test", 2}, {"is", 1}}
	wfm := WordFrequencyManager{}
	wfm.NewWordFrequencyManager()
	wfm.IncrementCount("test")
	wfm.IncrementCount("test")
	wfm.IncrementCount("is")
	sortedWordFreqs := wfm.Sorted()
	if !reflect.DeepEqual(sortedWordFreqs, want) {
		t.Errorf("Sort() = %v, want %v", sortedWordFreqs, want)
	}
}

func TestWordFrequencyControllerRun(t *testing.T) {
	oldStdout := os.Stdout
	r, w, _ := os.Pipe()
	os.Stdout = w

	want := "test - 1\n"
	wfc := WordFrequencyController{}
	wfc.NewWordFrequencyController("test.txt")
	wfc.Run()

	w.Close()
	var buf bytes.Buffer
	_, _ = buf.ReadFrom(r)
	os.Stdout = oldStdout

	if diff := cmp.Diff(want, buf.String()); diff != "" {
		t.Errorf("Output mismatch (-want +got):\n%s", diff)
	}
}
