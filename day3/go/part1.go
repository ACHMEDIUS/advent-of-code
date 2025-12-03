package main

import (
	"bufio"
	"fmt"
	"os"
)

func maxJoltage(line string) int {
	n := len(line)

	// Compute max digit from each position+1 to end (right to left)
	maxAfter := make([]int, n)
	maxSoFar := 0

	for i := n - 1; i >= 0; i-- {
		maxAfter[i] = maxSoFar
		digit := int(line[i] - '0')
		if digit > maxSoFar {
			maxSoFar = digit
		}
	}

	// Find best pair: digit[i] * 10 + maxAfter[i]
	best := 0
	for i := 0; i < n-1; i++ {
		digit := int(line[i] - '0')
		joltage := digit*10 + maxAfter[i]
		if joltage > best {
			best = joltage
		}
	}

	return best
}

func main() {
	filename := "input.txt"
	if len(os.Args) > 1 {
		filename = os.Args[1]
	}

	file, _ := os.Open(filename)
	defer file.Close()

	sum := 0
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		sum = sum + maxJoltage(line)
	}

	fmt.Println(sum)
}
