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

// Union-Find data structure
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
		uf.parent[x] = uf.Find(uf.parent[x]) // path compression
	}
	return uf.parent[x]
}

func (uf *UnionFind) Union(x, y int) {
	px, py := uf.Find(x), uf.Find(y)
	if px == py {
		return
	}
	// union by rank
	if uf.rank[px] < uf.rank[py] {
		uf.parent[px] = py
	} else if uf.rank[px] > uf.rank[py] {
		uf.parent[py] = px
	} else {
		uf.parent[py] = px
		uf.rank[px]++
	}
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

	// Connect 1000 closest pairs using Union-Find
	uf := NewUnionFind(n)
	connections := 1000
	for i := 0; i < connections && i < len(pairs); i++ {
		uf.Union(pairs[i].i, pairs[i].j)
	}

	// Count circuit sizes
	sizes := make(map[int]int)
	for i := 0; i < n; i++ {
		root := uf.Find(i)
		sizes[root]++
	}

	// Get all sizes and sort descending
	var sizeList []int
	for _, size := range sizes {
		sizeList = append(sizeList, size)
	}
	sort.Sort(sort.Reverse(sort.IntSlice(sizeList)))

	// Multiply top 3
	result := sizeList[0] * sizeList[1] * sizeList[2]
	fmt.Println(result)
}
