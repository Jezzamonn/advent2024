import java.lang.IllegalStateException
import java.util.PriorityQueue

data class Grid<T>(val grid: List<MutableList<T>>) {
    val width = grid[0].size
    val height = grid.size

    fun inBounds(point: Point): Boolean {
        return point.x in 0..<width && point.y in 0..<height
    }

    fun get(point: Point): T? {
        if (!inBounds(point)) {
            return null
        }
        return grid[point.y][point.x]
    }

    fun set(point: Point, value: T) {
        grid[point.y][point.x] = value
    }

    fun find(value: T): Point {
        for (y in 0..<height) {
            for (x in 0..<width) {
                if (grid[y][x] == value) {
                    return Point(x, y)
                }
            }
        }
        throw IllegalArgumentException("Value not found: $value")
    }

    companion object {
        fun <T> ofSize(width: Int, height: Int, fill: T): Grid<T> {
            val grid = List(height) { MutableList(width) { fill } }
            return Grid(grid)
        }
    }
}

data class Point(val x: Int, val y: Int) {
    fun neighbors(): List<Point> {
        return listOf(
            Point(x + 1, y),
            Point(x - 1, y),
            Point(x, y + 1),
            Point(x, y - 1)
        )
    }
}

data class SearchState(val point: Point, val cost: Int) {

    fun nextStates(grid: Grid<Boolean>): List<SearchState> {
        return point.neighbors().map { SearchState(it, cost + 1) }
            .filter { grid.get(it.point) == false }
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

val demoSize = 7
val actualSize = 71

val demoNumSteps = 12
val actualNumSteps = 1024

val size = actualSize
val steps = actualNumSteps

fun part1() {
    val grid = Grid.ofSize(size, size, false)

    val input = generateSequence(::readLine).map { it.split(",").map { num -> num.toInt() } }.toList()
    for (i in 0..<actualNumSteps) {
        val (x, y) = input[i]
        grid.set(Point(x, y), true)
    }

    // Then a simple BFS to find the shortest path
    val start = Point(0, 0)
    val end = Point(size - 1, size - 1)

    val queue = PriorityQueue<SearchState>(compareBy { it.cost })
    queue.add(SearchState(start, 0))
    val visited = mutableSetOf<Point>()

    while (queue.isNotEmpty()) {
        val state = queue.remove()
        if (state.point == end) {
            println(state.cost)
            return
        }
        if (visited.contains(state.point)) {
            continue
        }
        visited.add(state.point)
        queue.addAll(state.nextStates(grid))
    }
}


fun part2() {}
