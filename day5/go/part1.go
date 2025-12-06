package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	filename := "input.txt"
	if len(os.Args) > 1 {
		filename = os.Args[1]
	}

	file, _ := os.Open(filename)
	defer file.Close()

	var ranges [][2]int
	var ingredients []int

	scanner := bufio.NewScanner(file)
	readingRanges := true

	for scanner.Scan() {
		line := scanner.Text()

		// Blank line separates ranges from ingredients
		if line == "" {
			readingRanges = false
			continue
		}

		if readingRanges {
			// Parse range: "3-5"
			parts := strings.Split(line, "-")
			start, _ := strconv.Atoi(parts[0])
			end, _ := strconv.Atoi(parts[1])
			ranges = append(ranges, [2]int{start, end})
		} else {
			// Parse ingredient ID
			id, _ := strconv.Atoi(line)
			ingredients = append(ingredients, id)
		}
	}

	// Count fresh ingredients
	fresh := 0
	for _, id := range ingredients {
		for _, r := range ranges {
			if id >= r[0] && id <= r[1] {
				fresh = fresh + 1
				break
			}
		}
	}

	fmt.Println(fresh)
}
