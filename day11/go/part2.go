package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func main() {
	filename := "input.txt"
	if len(os.Args) > 1 {
		filename = os.Args[1]
	}
	file, _ := os.Open(filename)
	defer file.Close()

	graph := make(map[string][]string)

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		parts := strings.Split(line, ": ")
		node := parts[0]
		outputs := strings.Fields(parts[1])
		graph[node] = outputs
	}

	// State: (node, visitedMask) where mask tracks if dac(bit 0) and fft(bit 1) visited
	// memo[node][mask] = number of paths from node to out with that mask
	memo := make(map[string]map[int]int)

	var countPaths func(node string, mask int) int
	countPaths = func(node string, mask int) int {
		// Update mask based on current node
		if node == "dac" {
			mask |= 1
		}
		if node == "fft" {
			mask |= 2
		}

		if node == "out" {
			// Only count if both dac and fft were visited
			if mask == 3 {
				return 1
			}
			return 0
		}

		if memo[node] == nil {
			memo[node] = make(map[int]int)
		}
		if val, exists := memo[node][mask]; exists {
			return val
		}

		total := 0
		for _, next := range graph[node] {
			total += countPaths(next, mask)
		}

		memo[node][mask] = total
		return total
	}

	fmt.Println(countPaths("svr", 0))
}
