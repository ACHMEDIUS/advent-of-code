package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strconv"
	"strings"
)

type Point struct {
	x, y int
}

type Edge struct {
	horizontal bool
	x, y       int // for vertical: x coord; for horizontal: y coord
	start, end int // range (min to max)
}

func abs(n int) int {
	if n < 0 {
		return -n
	}
	return n
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

// Count vertical edge crossings for ray from (px, py) going right
func countCrossings(px, py int, vertEdges []Edge) int {
	count := 0
	for _, e := range vertEdges {
		// Vertical edge at x = e.x from y = e.start to y = e.end
		if e.x > px && py >= e.start && py < e.end {
			count++
		}
	}
	return count
}

func main() {
	filename := "input.txt"
	if len(os.Args) > 1 {
		filename = os.Args[1]
	}
	file, _ := os.Open(filename)
	defer file.Close()

	var redTiles []Point
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		parts := strings.Split(line, ",")
		x, _ := strconv.Atoi(parts[0])
		y, _ := strconv.Atoi(parts[1])
		redTiles = append(redTiles, Point{x, y})
	}

	n := len(redTiles)

	// Build edges from consecutive red tiles
	var horizEdges, vertEdges []Edge
	for i := 0; i < n; i++ {
		p1 := redTiles[i]
		p2 := redTiles[(i+1)%n]

		if p1.y == p2.y {
			// Horizontal edge
			horizEdges = append(horizEdges, Edge{
				horizontal: true,
				y:          p1.y,
				start:      min(p1.x, p2.x),
				end:        max(p1.x, p2.x),
			})
		} else {
			// Vertical edge
			vertEdges = append(vertEdges, Edge{
				horizontal: false,
				x:          p1.x,
				start:      min(p1.y, p2.y),
				end:        max(p1.y, p2.y),
			})
		}
	}

	// Sort edges for faster lookups
	sort.Slice(horizEdges, func(i, j int) bool {
		return horizEdges[i].y < horizEdges[j].y
	})
	sort.Slice(vertEdges, func(i, j int) bool {
		return vertEdges[i].x < vertEdges[j].x
	})

	// Check if rectangle is valid (fully inside polygon)
	isValidRect := func(x1, y1, x2, y2 int) bool {
		minX, maxX := min(x1, x2), max(x1, x2)
		minY, maxY := min(y1, y2), max(y1, y2)

		// Check if any horizontal edge passes through interior of rectangle
		for _, e := range horizEdges {
			if e.y > minY && e.y < maxY {
				// Edge y is strictly inside rectangle's y-range
				if e.start < maxX && e.end > minX {
					// Edge x-range overlaps with rectangle's x-range
					return false
				}
			}
		}

		// Check if any vertical edge passes through interior of rectangle
		for _, e := range vertEdges {
			if e.x > minX && e.x < maxX {
				// Edge x is strictly inside rectangle's x-range
				if e.start < maxY && e.end > minY {
					// Edge y-range overlaps with rectangle's y-range
					return false
				}
			}
		}

		// No edge cuts through interior - check if center is inside polygon
		centerX := (minX + maxX) / 2
		centerY := (minY + maxY) / 2

		// Use ray casting: count vertical edges to the right of center
		crossings := countCrossings(centerX, centerY, vertEdges)
		return crossings%2 == 1 // odd = inside
	}

	maxArea := 0
	for i := 0; i < n; i++ {
		for j := i + 1; j < n; j++ {
			p1, p2 := redTiles[i], redTiles[j]
			if isValidRect(p1.x, p1.y, p2.x, p2.y) {
				w := abs(p2.x-p1.x) + 1
				h := abs(p2.y-p1.y) + 1
				area := w * h
				if area > maxArea {
					maxArea = area
				}
			}
		}
	}

	fmt.Println(maxArea)
}
