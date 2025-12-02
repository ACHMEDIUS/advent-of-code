package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func isInvalid(n int) bool {
	s := strconv.Itoa(n)
	length := len(s)

	// Try each possible pattern length (must repeat at least twice)
	for patternLen := 1; patternLen <= length/2; patternLen++ {
		// Pattern length must divide total length evenly
		if length%patternLen != 0 {
			continue
		}

		pattern := s[:patternLen]
		repetitions := length / patternLen

		if strings.Repeat(pattern, repetitions) == s {
			return true
		}
	}

	return false
}

func main() {
	filename := "input.txt"
	if len(os.Args) > 1 {
		filename = os.Args[1]
	}

	file, _ := os.Open(filename)
	defer file.Close()

	scanner := bufio.NewScanner(file)
	scanner.Scan()
	line := scanner.Text()

	line = strings.TrimSuffix(line, ",")

	ranges := strings.Split(line, ",")
	sum := 0

	for _, r := range ranges {
		parts := strings.Split(r, "-")
		start, _ := strconv.Atoi(parts[0])
		end, _ := strconv.Atoi(parts[1])

		for n := start; n <= end; n++ {
			if isInvalid(n) {
				sum = sum + n
			}
		}
	}

	fmt.Println(sum)
}
