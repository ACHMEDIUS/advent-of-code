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

	// Must have even length
	if length%2 != 0 {
		return false
	}

	half := length / 2
	firstHalf := s[:half]
	secondHalf := s[half:]

	return firstHalf == secondHalf
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

	// Remove trailing comma if present
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
