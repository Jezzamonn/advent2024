package main

import (
	"bufio"
	"fmt"
	"os"
)

type Direction int

const (
	Up Direction = iota
	Left
	Down
	Right
)

func (d Direction) String() string {
	return [...]string{"Up", "Down", "Left", "Right"}[d]
}

func (d Direction) TurnRight() Direction {
	return (d + 3) % 4
}

func (d Direction) Delta() Point {
	switch d {
	case Up:
		return Point{0, -1}
	case Down:
		return Point{0, 1}
	case Left:
		return Point{-1, 0}
	case Right:
		return Point{1, 0}
	}
	return Point{}
}

type Point struct {
	X, Y int
}

func (p Point) Equal(other Point) bool {
	return p.X == other.X && p.Y == other.Y
}

func (p Point) Add(other Point) Point {
	return Point{p.X + other.X, p.Y + other.Y}
}

func (p Point) Neighbors() []Point {
	return []Point{
		{p.X - 1, p.Y},
		{p.X + 1, p.Y},
		{p.X, p.Y - 1},
		{p.X, p.Y + 1},
	}
}

func (p Point) InBounds(grid [][]rune) bool {
	return p.X >= 0 && p.X < len(grid[0]) && p.Y >= 0 && p.Y < len(grid)
}

type PointDir struct {
	Point
	Direction
}

func (pd PointDir) Forward() PointDir {
	return PointDir{pd.Point.Add(pd.Direction.Delta()), pd.Direction}
}

func (pd PointDir) TurnRight() PointDir {
	return PointDir{pd.Point, pd.Direction.TurnRight()}
}

func main() {
	var grid [][]rune
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		line := scanner.Text()
		grid = append(grid, []rune(line))
	}

	if err := scanner.Err(); err != nil {
		panic(err)
	}

	var start PointDir
	for y, row := range grid {
		for x, cell := range row {
			if cell == '^' {
				start = PointDir{Point{x, y}, Up}
			}
		}
	}

	visited := make(map[PointDir]struct{})
	current := start
	for {
		visited[current] = struct{}{}
		current = next(grid, current)
		if !current.InBounds(grid) {
			break
		}
	}

	visitedPoints := make(map[Point]struct{})
	for pd := range visited {
		visitedPoints[pd.Point] = struct{}{}
	}

	fmt.Println(len(visitedPoints))
}

func next(grid [][]rune, current PointDir) PointDir {
	next := current.Forward()
	if !next.InBounds(grid) {
		// Let the outer method handle it being out of bounds and end.
		return next
	}
	if grid[next.Y][next.X] == '#' {
		return current.TurnRight()
	}
	return next
}
