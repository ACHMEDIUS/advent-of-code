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

		// Count how many times we land on 0 during this rotation
		if direction == "L" {
			if position == 0 {
				count = count + distance/100
			} else if distance >= position {
				count = count + (distance-position)/100 + 1
			}
			position = position - distance
		} else {
			count = count + (position+distance)/100
			position = position + distance
		}

		// Wrap around 0-99
		position = ((position % 100) + 100) % 100
	}

	fmt.Println(count)
}
