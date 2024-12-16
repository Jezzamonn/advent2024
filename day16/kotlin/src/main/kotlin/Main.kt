import java.lang.IllegalStateException
import java.util.PriorityQueue

data class Grid(val grid: List<MutableList<Char>>) {
    val width = grid[0].size
    val height = grid.size

    fun inBounds(point: Point): Boolean {
        return point.x in 0..<width && point.y in 0..<height
    }

    fun get(point: Point): Char? {
        if (!inBounds(point)) {
            return null
        }
        return grid[point.y][point.x]
    }

    fun set(point: Point, value: Char) {
        grid[point.y][point.x] = value
    }

    fun find(char: Char): Point {
        for (y in 0..<height) {
            for (x in 0..<width) {
                if (grid[y][x] == char) {
                    return Point(x, y)
                }
            }
        }
        throw IllegalArgumentException("Char not found: $char")
    }
}

fun gridOf(input: String): Grid {
    val lines = input.split("\n")
    val grid = lines.map { it.toMutableList() }
    return Grid(grid)
}

enum class Direction {
    UP, DOWN, LEFT, RIGHT;

    fun delta(): Point {
        return when (this) {
            UP -> Point(0, -1)
            DOWN -> Point(0, 1)
            LEFT -> Point(-1, 0)
            RIGHT -> Point(1, 0)
        }
    }

    fun opposite(): Direction {
        return when (this) {
            UP -> DOWN
            DOWN -> UP
            LEFT -> RIGHT
            RIGHT -> LEFT
        }
    }

    fun turnLeft(): Direction {
        return when (this) {
            UP -> LEFT
            DOWN -> RIGHT
            LEFT -> DOWN
            RIGHT -> UP
        }
    }

    fun turnRight(): Direction {
        return when (this) {
            UP -> RIGHT
            DOWN -> LEFT
            LEFT -> UP
            RIGHT -> DOWN
        }
    }
}

fun directionOf(char: Char): Direction? {
    return when (char) {
        '^' -> Direction.UP
        'v' -> Direction.DOWN
        '<' -> Direction.LEFT
        '>' -> Direction.RIGHT
        else -> null
    }
}

data class Point(val x: Int, val y: Int) {
    fun move(direction: Direction): Point {
        return this + direction.delta()
    }

    operator fun plus(other: Point): Point {
        return Point(x + other.x, y + other.y)
    }
}

data class PointDir(val point: Point, val direction: Direction) {
    fun moveForward(): PointDir {
        return PointDir(point.move(direction), direction)
    }

    fun turnLeft(): PointDir {
        return PointDir(point, direction.turnLeft())
    }

    fun turnRight(): PointDir {
        return PointDir(point, direction.turnRight())
    }
}

data class SearchState(val pointDir: PointDir, val cost: Int) {

    fun nextStates(grid: Grid): List<SearchState> {
        return listOf(
            SearchState(pointDir.moveForward(), cost + 1),
            SearchState(pointDir.turnLeft(), cost + 1000),
            SearchState(pointDir.turnRight(), cost + 1000)
        ).filter { grid.inBounds(it.pointDir.point) && grid.get(it.pointDir.point) != '#' }
    }
}

fun main(args: Array<String>) {
    val argsJoined = args.joinToString(" ")
    when {
        "--part 1" in argsJoined -> part1()
        "--part 2" in argsJoined -> part2()
        else -> throw IllegalArgumentException("Missing or invalid --part argument")
    }
}

fun part1() {
    val input = generateSequence(::readLine).joinToString("\n")
    val grid = gridOf(input)

    val start = grid.find('S')
    val end = grid.find('E')

    // BFS
    val priorityQueue = PriorityQueue<SearchState> { a, b -> a.cost - b.cost }
    priorityQueue.add(SearchState(PointDir(start, Direction.LEFT), 0))

    val visited = mutableSetOf<PointDir>()
    while (priorityQueue.isNotEmpty()) {
        val current = priorityQueue.remove()
        if (visited.contains(current.pointDir)) {
            continue
        }
        visited.add(current.pointDir)

        if (current.pointDir.point == end) {
            println(current.cost)
            return
        }

        for (next in current.nextStates(grid)) {
            priorityQueue.add(next)
        }
    }
    throw IllegalStateException("No path found")
}

fun printAnimationFrame(grid: Grid, visited: Set<PointDir>, current: PointDir) {
    // Clear console
    print("\u001b[H\u001b[2J")

    // Print with colors
    // Wall = white
    // Empty = grey
    // Visited = green
    // Current = yellow
    val white = "\u001b[37m"
    val grey = "\u001b[90m"
    val green = "\u001b[32m"
    val yellow = "\u001b[33m"

    for (y in 0..<grid.height) {
        for (x in 0..<grid.width) {

            // Also, for visited, print the number of directions visited
            val point = Point(x, y)

            val numVisited = Direction.entries.count { visited.contains(PointDir(point, it)) }
            val char = grid.get(point)!!

            if (point == current.point) {
                print("${yellow}@")
            } else if (visited.contains(PointDir(point, current.direction))) {
                print("$green$numVisited")
            } else {
                when (char) {
                    '#' -> print("$white$char")
                    '.' -> print("$grey$char")
                    else -> print(char)
                }
            }
        }
        println()
    }
    println()

    // Sleep for 100ms
    Thread.sleep(100)
}

fun part2() {
    val input = generateSequence(::readLine).joinToString("\n")
    val grid = gridOf(input)

    val start = grid.find('S')
    val end = grid.find('E')

    // BFS
    val priorityQueue = PriorityQueue<SearchState> { a, b -> a.cost - b.cost }
    priorityQueue.add(SearchState(PointDir(start, Direction.LEFT), 0))

    val visited = mutableSetOf<PointDir>()
    while (priorityQueue.isNotEmpty()) {
        val current = priorityQueue.remove()
        if (visited.contains(current.pointDir)) {
            continue
        }
        visited.add(current.pointDir)

        if (current.pointDir.point == end) {
            println(current.cost)
            return
        }

        for (next in current.nextStates(grid)) {
            priorityQueue.add(next)
        }
    }
    throw IllegalStateException("No path found")
}
