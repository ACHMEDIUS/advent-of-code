package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func main() {
	file, _ := os.Open("input.txt")
	defer file.Close()

	position := 50
	count := 0

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()

		direction := string(line[0])
		distance, _ := strconv.Atoi(line[1:])

		if direction == "L" {
			position = position - distance
		} else {
			position = position + distance
		}

		// Wrap around 0-99
		position = ((position % 100) + 100) % 100

		if position == 0 {
			count++
		}
	}

	fmt.Println(count)
}
