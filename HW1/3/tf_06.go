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

func main() {
	PrintAll(Sort(Frequencies(RemoveStopWords(Scan(FilterCharsAndNormalize(ReadFile("../../pride-and-prejudice.txt"))))("../../stop_words.txt")))[0:25])
}

func ReadFile(pathToFile string) string {
	dat, _ := os.ReadFile(pathToFile)
	return string(dat)
}

func FilterCharsAndNormalize(strData string) string {
	r, _ := regexp.Compile("[^a-zA-Z0-9_]+")
	return strings.ToLower(r.ReplaceAllString(strData, " "))
}

func Scan(str_data string) []string {
	return strings.Split(str_data, " ")
}

func RemoveStopWords(words []string) func(string) []string {
	return func(path string) []string {
		stopWords := strings.Split(ReadFile(path), ",")
		for i := 'a'; i <= 'z'; i++ {
			stopWords = append(stopWords, string(i))
		}

		var newWords []string
		for _, word := range words {
			if !slices.Contains(stopWords, word) {
				newWords = append(newWords, word)
			}
		}
		return newWords
	}
}

func Frequencies(words []string) map[string]int {
	wordFreqs := make(map[string]int)
	for _, word := range words {
		wordFreqs[word]++
	}
	return wordFreqs
}

func Sort(wordFreqs map[string]int) []pair {
	// Sort the map by value
	keys := make([]string, 0, len(wordFreqs))
	result := make([]pair, 0, len(wordFreqs))

	for key := range wordFreqs {
		keys = append(keys, key)
	}

	sort.SliceStable(keys, func(i, j int) bool {
		return wordFreqs[keys[i]] > wordFreqs[keys[j]]
	})

	for _, key := range keys {
		result = append(result, pair{key, wordFreqs[key]})
	}
	return result
}

func PrintAll(wordFreqs []pair) {
	for _, pair := range wordFreqs {
		fmt.Println(pair.key, "-", pair.value)
	}
}
