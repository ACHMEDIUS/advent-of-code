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

	var lines []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}

	// Pad all lines to same length
	maxLen := 0
	for _, line := range lines {
		if len(line) > maxLen {
			maxLen = len(line)
		}
	}
	for i := range lines {
		for len(lines[i]) < maxLen {
			lines[i] += " "
		}
	}

	// Find separator columns (all spaces)
	separators := make([]bool, maxLen)
	for col := 0; col < maxLen; col++ {
		allSpaces := true
		for row := 0; row < len(lines); row++ {
			if lines[row][col] != ' ' {
				allSpaces = false
				break
			}
		}
		separators[col] = allSpaces
	}

	// Extract problem ranges
	var problems [][2]int
	start := -1
	for col := 0; col < maxLen; col++ {
		if separators[col] {
			if start != -1 {
				problems = append(problems, [2]int{start, col})
				start = -1
			}
		} else {
			if start == -1 {
				start = col
			}
		}
	}
	if start != -1 {
		problems = append(problems, [2]int{start, maxLen})
	}

	// Solve each problem
	grandTotal := 0
	for _, prob := range problems {
		start, end := prob[0], prob[1]

		// Extract numbers from rows 0 to len-2
		var numbers []int
		for row := 0; row < len(lines)-1; row++ {
			numStr := strings.TrimSpace(lines[row][start:end])
			if numStr != "" {
				num, _ := strconv.Atoi(numStr)
				numbers = append(numbers, num)
			}
		}

		// Extract operator from last row
		opStr := strings.TrimSpace(lines[len(lines)-1][start:end])

		// Calculate result
		result := numbers[0]
		if opStr == "*" {
			for i := 1; i < len(numbers); i++ {
				result = result * numbers[i]
			}
		} else {
			for i := 1; i < len(numbers); i++ {
				result = result + numbers[i]
			}
		}

		grandTotal = grandTotal + result
	}

	fmt.Println(grandTotal)
}
