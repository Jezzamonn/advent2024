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

    fun toStringWithColors(): String {
        // O = box, in blue
        // # = wall, in white
        // . = free space, in grey
        // @ = robot, in yellow

        val colorReset = "\u001B[0m"
        val colorBlue = "\u001B[34m"
        val colorWhite = "\u001B[37m"
        val colorGrey = "\u001B[90m"
        val colorYellow = "\u001B[33m"

        return grid.joinToString("\n") { row ->
            row.joinToString("") { char ->
                when (char) {
                    'O' -> "$colorBlue$char$colorReset"
                    '#' -> "$colorWhite$char$colorReset"
                    '.' -> "$colorGrey$char$colorReset"
                    '@' -> "$colorYellow$char$colorReset"
                    else -> char.toString()
                }
            }
        }
    }

    fun score(): Int {
        return grid.withIndex().sumOf { (y, row) ->
            row.withIndex().sumOf { (x, char) ->
                when (char) {
                    'O' -> 100 * y + x
                    else -> 0
                }
            }
        }
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

    val (gridStr, instructions) = input.split("\n\n")
    val grid = gridOf(gridStr)

    var robotPos = grid.find('@')
    instructionLoop@ for (instruction in instructions) {
        val direction = directionOf(instruction) ?: continue

        // Scan until we find a free space. Blocks mean we continue, wall means we break and don't move.
        var endOfPush = robotPos.move(direction)
        while (true) {
            val nextChar = grid.get(endOfPush)
            when (nextChar) {
                '#' -> continue@instructionLoop
                '.' -> break
                null -> break
            }
            endOfPush = endOfPush.move(direction)
        }
        // Now go backwards and move tiles.
        while (endOfPush != robotPos) {
            val prevPos = endOfPush.move(direction.opposite())
            grid.set(endOfPush, grid.get(prevPos)!!)
            endOfPush = prevPos
        }
        grid.set(robotPos, '.')
        robotPos = robotPos.move(direction)

//        // Clear terminal
//        print("\u001b[H\u001b[2J")
//        println(grid.toStringWithColors())
//        println("Last instruction: $instruction")
//        // Sleep so we can see it
//        Thread.sleep(50)
    }

    println("Score: ${grid.score()}")
}

fun part2() {
}

