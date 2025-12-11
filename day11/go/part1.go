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

	// Build adjacency list
	graph := make(map[string][]string)

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		parts := strings.Split(line, ": ")
		node := parts[0]
		outputs := strings.Fields(parts[1])
		graph[node] = outputs
	}

	// Count paths from "you" to "out" using memoization
	memo := make(map[string]int)

	var countPaths func(node string) int
	countPaths = func(node string) int {
		if node == "out" {
			return 1
		}

		if val, exists := memo[node]; exists {
			return val
		}

		total := 0
		for _, next := range graph[node] {
			total += countPaths(next)
		}

		memo[node] = total
		return total
	}

	fmt.Println(countPaths("you"))
}
