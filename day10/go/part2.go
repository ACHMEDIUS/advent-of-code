package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

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

func solve(buttons [][]int, targets []int) int {
	n := len(targets)  // counters
	m := len(buttons)  // buttons

	matrix := make([][]int, n)
	for i := 0; i < n; i++ {
		matrix[i] = make([]int, m+1)
		matrix[i][m] = targets[i]
	}
	for j, button := range buttons {
		for _, idx := range button {
			if idx < n {
				matrix[idx][j] = 1
			}
		}
	}

	// Gaussian elimination (integer arithmetic)
	pivotCols := make([]int, 0)
	pivotRows := make([]int, 0)
	row := 0

	for col := 0; col < m && row < n; col++ {
		// Find pivot
		pivotRow := -1
		for i := row; i < n; i++ {
			if matrix[i][col] != 0 {
				pivotRow = i
				break
			}
		}
		if pivotRow == -1 {
			continue // no pivot in this column
		}

		// Swap rows
		matrix[row], matrix[pivotRow] = matrix[pivotRow], matrix[row]

		// Eliminate
		for i := 0; i < n; i++ {
			if i != row && matrix[i][col] != 0 {
				// Subtract matrix[i][col]/matrix[row][col] * row from row i
				// To stay in integers, multiply row i by matrix[row][col] first
				mult := matrix[row][col]
				sub := matrix[i][col]
				for j := 0; j <= m; j++ {
					matrix[i][j] = matrix[i][j]*mult - sub*matrix[row][j]
				}
			}
		}

		pivotCols = append(pivotCols, col)
		pivotRows = append(pivotRows, row)
		row++
	}

	// Identify free variables (non-pivot columns)
	isPivot := make([]bool, m)
	for _, col := range pivotCols {
		isPivot[col] = true
	}
	freeVars := make([]int, 0)
	for j := 0; j < m; j++ {
		if !isPivot[j] {
			freeVars = append(freeVars, j)
		}
	}

	numFree := len(freeVars)
	maxTarget := 0
	for _, t := range targets {
		maxTarget = max(maxTarget, t)
	}

	best := -1

	// Search over free variable values
	var searchFree func(idx int, freeVals []int)
	searchFree = func(idx int, freeVals []int) {
		if idx == numFree {
			// Compute basic variables from free variables
			solution := make([]int, m)
			for i, fv := range freeVars {
				solution[fv] = freeVals[i]
			}

			// Back-substitute to find basic variables
			valid := true
			for i := len(pivotCols) - 1; i >= 0 && valid; i-- {
				r := pivotRows[i]
				c := pivotCols[i]

				// matrix[r][c] * solution[c] + sum of other terms = matrix[r][m]
				rhs := matrix[r][m]
				for j := 0; j < m; j++ {
					if j != c {
						rhs -= matrix[r][j] * solution[j]
					}
				}
				if rhs%matrix[r][c] != 0 {
					valid = false
				} else {
					solution[c] = rhs / matrix[r][c]
					if solution[c] < 0 {
						valid = false
					}
				}
			}

			if valid {
				sum := 0
				for _, v := range solution {
					sum += v
				}
				if best < 0 || sum < best {
					best = sum
				}
			}
			return
		}

		// Prune based on current best
		for v := 0; v <= maxTarget; v++ {
			if best >= 0 {
				// Current sum of free vars
				currentSum := v
				for i := 0; i < idx; i++ {
					currentSum += freeVals[i]
				}
				if currentSum >= best {
					break
				}
			}
			freeVals[idx] = v
			searchFree(idx+1, freeVals)
		}
	}

	if numFree == 0 {
		// No free variables - unique solution
		searchFree(0, []int{})
	} else if numFree <= 5 {
		// Few free variables - search
		searchFree(0, make([]int, numFree))
	} else {
		// Too many free variables - try a different approach
		// Just try small values
		searchFree(0, make([]int, numFree))
	}

	return best
}

func main() {
	filename := "input.txt"
	if len(os.Args) > 1 {
		filename = os.Args[1]
	}
	file, _ := os.Open(filename)
	defer file.Close()

	totalPresses := 0

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()

		var buttons [][]int
		inParen := false
		var current string
		for _, c := range line {
			if c == '(' {
				inParen = true
				current = ""
			} else if c == ')' {
				inParen = false
				var indices []int
				for _, s := range strings.Split(current, ",") {
					idx, _ := strconv.Atoi(s)
					indices = append(indices, idx)
				}
				buttons = append(buttons, indices)
			} else if inParen {
				current += string(c)
			}
		}

		var targets []int
		inCurly := false
		current = ""
		for _, c := range line {
			if c == '{' {
				inCurly = true
				current = ""
			} else if c == '}' {
				inCurly = false
				for _, s := range strings.Split(current, ",") {
					val, _ := strconv.Atoi(s)
					targets = append(targets, val)
				}
			} else if inCurly {
				current += string(c)
			}
		}

		result := solve(buttons, targets)
		if result >= 0 {
			totalPresses += result
		}
	}

	fmt.Println(totalPresses)
}
