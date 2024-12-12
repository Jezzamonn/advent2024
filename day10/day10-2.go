package main

import (
	"bufio"
	"fmt"
	"os"
	"time"
)

type Point struct {
	X, Y int
}

func (p Point) Equal(other Point) bool {
	return p.X == other.X && p.Y == other.Y
}

func (p Point) Neighbors() []Point {
	return []Point{
		{p.X - 1, p.Y},
		{p.X + 1, p.Y},
		{p.X, p.Y - 1},
		{p.X, p.Y + 1},
	}
}

func (p Point) InBounds(grid [][]int) bool {
	return p.X >= 0 && p.X < len(grid[0]) && p.Y >= 0 && p.Y < len(grid)
}

func main() {
	var grid [][]int
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		line := scanner.Text()
		row := make([]int, len(line))
		for i, c := range line {
			row[i] = int(c - '0')
		}
		grid = append(grid, row)
	}
	if err := scanner.Err(); err != nil {
		panic(err)
	}

	// // Print the grid
	// for _, row := range grid {
	// 	fmt.Println(row)
	// }

	sum := 0
	for y := 0; y < len(grid); y++ {
		for x := 0; x < len(grid[y]); x++ {
			// Count reachable 9's
			if grid[y][x] != 0 {
				continue
			}

			score := 0

			toVisit := []Point{{x, y}}
			for len(toVisit) > 0 {
				p := toVisit[0]
				toVisit = toVisit[1:]

				heightAtPoint := grid[p.Y][p.X]

				if heightAtPoint == 9 {
					score++
				}

				for _, neighbor := range p.Neighbors() {
					if !neighbor.InBounds(grid) {
						continue
					}
					heightAtNeighbor := grid[neighbor.Y][neighbor.X]
					if heightAtNeighbor == heightAtPoint+1 {
						toVisit = append(toVisit, neighbor)
						// printWithColors(grid, visited, Point{x, y})
					}
				}
			}
			sum += score
		}
	}
	fmt.Println(sum)
}

func printWithColors(grid [][]int, visited [][]bool, current Point) {
	// Clear terminal
	fmt.Print("\033[H\033[2J")

	for y, row := range grid {
		for x, cell := range row {
			if current.Equal(Point{x, y}) {
				// Print current point in red
				fmt.Print("\033[31m")
			} else if visited[y][x] {
				// Print visited points in green
				fmt.Print("\033[32m")
			} else {
				// Print other points in grey
				fmt.Print("\033[37m")
			}
			fmt.Print(cell)
		}
		fmt.Println()
	}

	// Sleep
	time.Sleep(10 * time.Millisecond)
}
