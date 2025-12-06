package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
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

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()

		if line == "" {
			break
		}

		parts := strings.Split(line, "-")
		start, _ := strconv.Atoi(parts[0])
		end, _ := strconv.Atoi(parts[1])
		ranges = append(ranges, [2]int{start, end})
	}

	// Sort ranges by start
	sort.Slice(ranges, func(i, j int) bool {
		return ranges[i][0] < ranges[j][0]
	})

	// Merge overlapping/touching ranges
	var merged [][2]int
	current := ranges[0]

	for i := 1; i < len(ranges); i++ {
		r := ranges[i]
		if r[0] <= current[1]+1 {
			// Overlaps or touches, merge
			if r[1] > current[1] {
				current[1] = r[1]
			}
		} else {
			// No overlap, save current and start new
			merged = append(merged, current)
			current = r
		}
	}
	merged = append(merged, current)

	// Count total IDs in merged ranges
	total := 0
	for _, r := range merged {
		total = total + (r[1] - r[0] + 1)
	}

	fmt.Println(total)
}
