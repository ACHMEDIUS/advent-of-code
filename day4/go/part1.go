package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	filename := "input.txt"
	if len(os.Args) > 1 {
		filename = os.Args[1]
	}

	file, _ := os.Open(filename)
	defer file.Close()

	// Read grid
	var grid []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		grid = append(grid, scanner.Text())
	}

	rows := len(grid)
	cols := len(grid[0])

	// 8 directions: up-left, up, up-right, left, right, down-left, down, down-right
	dirs := [][]int{
		{-1, -1}, {-1, 0}, {-1, 1},
		{0, -1}, {0, 1},
		{1, -1}, {1, 0}, {1, 1},
	}

	accessible := 0
	for r := 0; r < rows; r++ {
		for c := 0; c < cols; c++ {
			if grid[r][c] != '@' {
				continue
			}

			// Count @ neighbors
			neighbors := 0
			for _, d := range dirs {
				nr := r + d[0]
				nc := c + d[1]

				// Check bounds
				if nr >= 0 && nr < rows && nc >= 0 && nc < cols {
					if grid[nr][nc] == '@' {
						neighbors = neighbors + 1
					}
				}
			}

			if neighbors < 4 {
				accessible = accessible + 1
			}
		}
	}

	fmt.Println(accessible)
}
