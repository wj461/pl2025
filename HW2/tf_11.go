package main

import (
	"fmt"
	"os"
	"regexp"
	"slices"
	"sort"
	"strings"
)

type pair struct {
	key   string
	value int
}

type DataStorageManager struct {
	strData string
}

func (dsm *DataStorageManager) NewDataStorageManager(pathToFile string) {
	data, _ := os.ReadFile(pathToFile)
	r, _ := regexp.Compile(`[\W_]+`)
	dsm.strData = strings.ToLower(r.ReplaceAllString(string(data), " "))
}

func (dsm *DataStorageManager) Words() []string {
	return strings.Fields(dsm.strData)
}

type StopWordManager struct {
	stopWords []string
}

func (swm *StopWordManager) NewStopWordManager() {
	data, _ := os.ReadFile("../stop_words.txt")
	swm.stopWords = strings.Split(string(data), ",")
	for i := 'a'; i <= 'z'; i++ {
		swm.stopWords = append(swm.stopWords, string(i))
	}
}

func (swm *StopWordManager) IsStopWord(word string) bool {
	return slices.Contains(swm.stopWords, word)
}

type WordFrequencyManager struct {
	wordFreqs map[string]int
}

func (wfm *WordFrequencyManager) NewWordFrequencyManager() {
	wfm.wordFreqs = make(map[string]int)
}

func (wfm *WordFrequencyManager) IncrementCount(word string) {
	if _, ok := wfm.wordFreqs[word]; ok {
		wfm.wordFreqs[word]++
	} else {
		wfm.wordFreqs[word] = 1
	}
}

func (wfm *WordFrequencyManager) Sorted() []pair {
	// Sort the map by value
	keys := make([]string, 0, len(wfm.wordFreqs))
	result := make([]pair, 0, len(wfm.wordFreqs))

	for key := range wfm.wordFreqs {
		keys = append(keys, key)
	}

	sort.SliceStable(keys, func(i, j int) bool {
		return wfm.wordFreqs[keys[i]] > wfm.wordFreqs[keys[j]]
	})

	for _, key := range keys {
		result = append(result, pair{key, wfm.wordFreqs[key]})
	}
	return result

}

type WordFrequencyController struct {
	dataStorageManager   DataStorageManager
	stopWordManager      StopWordManager
	wordFrequencyManager WordFrequencyManager
}

func (wfc *WordFrequencyController) NewWordFrequencyController(pathToFile string) {
	wfc.dataStorageManager.NewDataStorageManager(pathToFile)
	wfc.stopWordManager.NewStopWordManager()
	wfc.wordFrequencyManager.NewWordFrequencyManager()
}

func (wfc *WordFrequencyController) Run() {
	for _, word := range wfc.dataStorageManager.Words() {
		if !wfc.stopWordManager.IsStopWord(word) {
			wfc.wordFrequencyManager.IncrementCount(word)
		}
	}

	wordFreqs := wfc.wordFrequencyManager.Sorted()
	for i := 0; i < 25; i++ {
		if i >= len(wordFreqs) {
			break
		}
		pair := wordFreqs[i]
		fmt.Print(pair.key, " - ", pair.value, "\n")
	}
}

func main() {
	wfc := WordFrequencyController{}
	wfc.NewWordFrequencyController(os.Args[1])
	wfc.Run()
}
