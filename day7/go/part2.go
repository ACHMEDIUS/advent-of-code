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

	var grid []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		grid = append(grid, scanner.Text())
	}

	rows := len(grid)
	cols := len(grid[0])

	// Find S
	startCol := 0
	for c := 0; c < cols; c++ {
		if grid[0][c] == 'S' {
			startCol = c
			break
		}
	}

	// Track particle counts at each position
	type pos struct{ r, c int }
	particles := map[pos]int{{0, startCol}: 1}
	totalTimelines := 0

	for len(particles) > 0 {
		newParticles := map[pos]int{}

		for p, count := range particles {
			r, c := p.r, p.c
			hitSplitter := false

			// Move down
			for r = r + 1; r < rows; r++ {
				if grid[r][c] == '^' {
					hitSplitter = true
					if c > 0 {
						newParticles[pos{r, c - 1}] += count
					}
					if c < cols-1 {
						newParticles[pos{r, c + 1}] += count
					}
					break
				}
			}

			if !hitSplitter {
				totalTimelines += count
			}
		}

		particles = newParticles
	}

	fmt.Println(totalTimelines)
}
