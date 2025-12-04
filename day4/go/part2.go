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

	// Read grid as mutable runes
	var grid [][]rune
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		grid = append(grid, []rune(scanner.Text()))
	}

	rows := len(grid)
	cols := len(grid[0])

	dirs := [][]int{
		{-1, -1}, {-1, 0}, {-1, 1},
		{0, -1}, {0, 1},
		{1, -1}, {1, 0}, {1, 1},
	}

	totalRemoved := 0

	for {
		// Find all accessible rolls
		var toRemove [][]int

		for r := 0; r < rows; r++ {
			for c := 0; c < cols; c++ {
				if grid[r][c] != '@' {
					continue
				}

				neighbors := 0
				for _, d := range dirs {
					nr := r + d[0]
					nc := c + d[1]

					if nr >= 0 && nr < rows && nc >= 0 && nc < cols {
						if grid[nr][nc] == '@' {
							neighbors = neighbors + 1
						}
					}
				}

				if neighbors < 4 {
					toRemove = append(toRemove, []int{r, c})
				}
			}
		}

		// If nothing to remove, we're done
		if len(toRemove) == 0 {
			break
		}

		// Remove all accessible rolls
		for _, pos := range toRemove {
			grid[pos[0]][pos[1]] = '.'
		}

		totalRemoved = totalRemoved + len(toRemove)
	}

	fmt.Println(totalRemoved)
}
