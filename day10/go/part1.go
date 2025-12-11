package main

import (
	"bufio"
	"fmt"
	"math/bits"
	"os"
	"regexp"
	"strconv"
	"strings"
)

func main() {
	filename := "input.txt"
	if len(os.Args) > 1 {
		filename = os.Args[1]
	}
	file, _ := os.Open(filename)
	defer file.Close()

	// Regex patterns
	diagramRe := regexp.MustCompile(`\[([.#]+)\]`)
	buttonRe := regexp.MustCompile(`\(([0-9,]+)\)`)

	totalPresses := 0

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()

		// Parse diagram - target state
		diagramMatch := diagramRe.FindStringSubmatch(line)
		diagram := diagramMatch[1]
		numLights := len(diagram)

		// Target state as bitmask
		var target uint64
		for i, c := range diagram {
			if c == '#' {
				target |= (1 << i)
			}
		}

		// Parse buttons - each button is a bitmask of which lights it toggles
		buttonMatches := buttonRe.FindAllStringSubmatch(line, -1)
		var buttons []uint64
		for _, match := range buttonMatches {
			var buttonMask uint64
			indices := strings.Split(match[1], ",")
			for _, idx := range indices {
				i, _ := strconv.Atoi(idx)
				if i < numLights {
					buttonMask |= (1 << i)
				}
			}
			buttons = append(buttons, buttonMask)
		}

		// Try all combinations of button presses (2^numButtons)
		numButtons := len(buttons)
		minPresses := numButtons + 1 // worst case: impossible

		for combo := 0; combo < (1 << numButtons); combo++ {
			// Compute resulting state by XORing selected buttons
			var state uint64
			for b := 0; b < numButtons; b++ {
				if (combo>>b)&1 == 1 {
					state ^= buttons[b]
				}
			}

			if state == target {
				presses := bits.OnesCount(uint(combo))
				if presses < minPresses {
					minPresses = presses
				}
			}
		}

		totalPresses += minPresses
	}

	fmt.Println(totalPresses)
}
