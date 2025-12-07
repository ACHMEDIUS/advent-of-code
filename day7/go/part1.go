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

	// Active beams: use map as set to dedupe
	type pos struct{ r, c int }
	beams := map[pos]bool{{0, startCol}: true}
	hitSplitters := map[pos]bool{}

	for len(beams) > 0 {
		newBeams := map[pos]bool{}

		for beam := range beams {
			r, c := beam.r, beam.c

			// Move beam down
			for r = r + 1; r < rows; r++ {
				if grid[r][c] == '^' {
					splitterPos := pos{r, c}
					if !hitSplitters[splitterPos] {
						hitSplitters[splitterPos] = true
						if c > 0 {
							newBeams[pos{r, c - 1}] = true
						}
						if c < cols-1 {
							newBeams[pos{r, c + 1}] = true
						}
					}
					break
				}
			}
		}

		beams = newBeams
	}

	fmt.Println(len(hitSplitters))
}
