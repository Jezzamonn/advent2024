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

data class SearchState(val pointDir: PointDir, val cost: Int, val previous: SearchState? = null) {

    fun nextStates(grid: Grid): List<SearchState> {
        return listOf(
            SearchState(pointDir.moveForward(), cost + 1, this),
            SearchState(pointDir.turnLeft(), cost + 1000, this),
            SearchState(pointDir.turnRight(), cost + 1000, this)
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
            } else if (numVisited > 0) {
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

    Thread.sleep(100)
}

fun printInBestPath(grid: Grid, inBestPath: Set<Point>) {
    // Clear console
    print("\u001b[H\u001b[2J")

    // Print with colors
    // Wall = white
    // Empty = grey
    // InBestPath = yellow
    val white = "\u001b[37m"
    val grey = "\u001b[90m"
    val yellow = "\u001b[33m"

    for (y in 0..<grid.height) {
        for (x in 0..<grid.width) {

            // Also, for visited, print the number of directions visited
            val point = Point(x, y)

            val char = grid.get(point)!!

            if (inBestPath.contains(point)) {
                print("${yellow}O")
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
}

fun part2() {
    val input = generateSequence(::readLine).joinToString("\n")
    val grid = gridOf(input)

    val start = grid.find('S')
    val end = grid.find('E')

    // BFS
    val priorityQueue = PriorityQueue<SearchState> { a, b -> a.cost - b.cost }
    priorityQueue.add(SearchState(PointDir(start, Direction.LEFT), 0))

    var bestCost: Int? = null
    val bestScore = mutableMapOf<PointDir, Int>()
    val previous = mutableMapOf<PointDir, MutableList<PointDir?>>()

    while (priorityQueue.isNotEmpty()) {
        val current = priorityQueue.remove()
        if (bestScore.contains(current.pointDir)) {
            if (current.cost > bestScore[current.pointDir]!!) {
                continue
            }
            previous[current.pointDir]!!.add(current.previous?.pointDir)
        }
        else {
            bestScore[current.pointDir] = current.cost
            previous[current.pointDir] = mutableListOf(current.previous?.pointDir)
        }

//        printAnimationFrame(grid, bestScore.keys, current.pointDir)

        if (current.cost > (bestCost ?: Int.MAX_VALUE)) {
            break
        }

        if (current.pointDir.point == end) {
            if (bestCost == null) {
                bestCost = current.cost
            }
            continue
        }

        for (next in current.nextStates(grid)) {
            priorityQueue.add(next)
        }
    }

    // Count tiles in any best path
    val inBestPath = mutableSetOf<Point>()
    val toVisit = Direction.entries.map { PointDir(end, it) }.toMutableList()
    val visited = mutableSetOf<PointDir>()
    while (toVisit.isNotEmpty()) {
//        printInBestPath(grid, inBestPath)

        val current = toVisit.removeLast()
        visited.add(current)
//        println(toVisit.size)

        // Sleep for 100ms
//        Thread.sleep(50)

        inBestPath.add(current.point)

        for (next in previous[current] ?: emptyList()) {
            if (next != null && !visited.contains(next)) {
                toVisit.add(next)
            }
        }
    }

    printInBestPath(grid, inBestPath)
    println(inBestPath.size)
}
