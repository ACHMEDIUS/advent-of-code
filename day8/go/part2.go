package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
	"sort"
	"strconv"
	"strings"
)

type Point struct {
	x, y, z int
}

type Pair struct {
	i, j int
	dist float64
}

type UnionFind struct {
	parent []int
	rank   []int
}

func NewUnionFind(n int) *UnionFind {
	parent := make([]int, n)
	rank := make([]int, n)
	for i := 0; i < n; i++ {
		parent[i] = i
	}
	return &UnionFind{parent, rank}
}

func (uf *UnionFind) Find(x int) int {
	if uf.parent[x] != x {
		uf.parent[x] = uf.Find(uf.parent[x])
	}
	return uf.parent[x]
}

// Union returns true if a merge happened (they were in different circuits)
func (uf *UnionFind) Union(x, y int) bool {
	px, py := uf.Find(x), uf.Find(y)
	if px == py {
		return false // already same circuit
	}
	if uf.rank[px] < uf.rank[py] {
		uf.parent[px] = py
	} else if uf.rank[px] > uf.rank[py] {
		uf.parent[py] = px
	} else {
		uf.parent[py] = px
		uf.rank[px]++
	}
	return true
}

func distance(a, b Point) float64 {
	dx := float64(a.x - b.x)
	dy := float64(a.y - b.y)
	dz := float64(a.z - b.z)
	return math.Sqrt(dx*dx + dy*dy + dz*dz)
}

func main() {
	file, _ := os.Open("input.txt")
	defer file.Close()

	var points []Point
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		parts := strings.Split(line, ",")
		x, _ := strconv.Atoi(parts[0])
		y, _ := strconv.Atoi(parts[1])
		z, _ := strconv.Atoi(parts[2])
		points = append(points, Point{x, y, z})
	}

	n := len(points)

	// Calculate all pairwise distances
	var pairs []Pair
	for i := 0; i < n; i++ {
		for j := i + 1; j < n; j++ {
			pairs = append(pairs, Pair{i, j, distance(points[i], points[j])})
		}
	}

	// Sort by distance
	sort.Slice(pairs, func(a, b int) bool {
		return pairs[a].dist < pairs[b].dist
	})

	// Connect pairs until all in one circuit
	// We need n-1 successful merges to connect n nodes
	uf := NewUnionFind(n)
	merges := 0
	var lastI, lastJ int

	for _, pair := range pairs {
		if uf.Union(pair.i, pair.j) {
			merges++
			lastI, lastJ = pair.i, pair.j
			if merges == n-1 {
				break // all connected
			}
		}
	}

	// Multiply X coordinates of last two connected boxes
	result := points[lastI].x * points[lastJ].x
	fmt.Println(result)
}
