package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func maxJoltage12(line string) int64 {
	n := len(line)
	k := 12 // digits to pick

	result := ""
	lastPos := -1

	for i := 0; i < k; i++ {
		// For digit i, need to leave k-1-i digits after
		// So search from lastPos+1 to n-k+i
		bestDigit := byte('0')
		bestPos := -1

		for j := lastPos + 1; j <= n-k+i; j++ {
			if line[j] > bestDigit {
				bestDigit = line[j]
				bestPos = j
			}
		}

		result = result + string(bestDigit)
		lastPos = bestPos
	}

	num, _ := strconv.ParseInt(result, 10, 64)
	return num
}

func main() {
	filename := "input.txt"
	if len(os.Args) > 1 {
		filename = os.Args[1]
	}

	file, _ := os.Open(filename)
	defer file.Close()

	var sum int64 = 0
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		sum = sum + maxJoltage12(line)
	}

	fmt.Println(sum)
}
