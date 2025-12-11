package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Point struct {
	x, y int
}

func abs(n int) int {
	if n < 0 {
		return -n
	}
	return n
}

func main() {
	filename := "input.txt"
	if len(os.Args) > 1 {
		filename = os.Args[1]
	}
	file, _ := os.Open(filename)
	defer file.Close()

	var tiles []Point
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		parts := strings.Split(line, ",")
		x, _ := strconv.Atoi(parts[0])
		y, _ := strconv.Atoi(parts[1])
		tiles = append(tiles, Point{x, y})
	}

	maxArea := 0

	// Try all pairs of tiles as opposite corners
	for i := 0; i < len(tiles); i++ {
		for j := i + 1; j < len(tiles); j++ {
			width := abs(tiles[j].x-tiles[i].x) + 1
			height := abs(tiles[j].y-tiles[i].y) + 1
			area := width * height

			if area > maxArea {
				maxArea = area
			}
		}
	}

	fmt.Println(maxArea)
}
