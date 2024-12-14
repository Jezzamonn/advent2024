//// For the demo
//const val width = 11
//const val height = 7

// Actual values
val width = 101
val height = 103

data class Robot(
    var x: Int,
    var y: Int,
    val vx: Int,
    val vy: Int
) {
    fun step() {
        x = ((x + vx) % width + width) % width
        y = ((y + vy) % height + height) % height
    }

    fun quadrant(): Int {
        if (x == width / 2 || y == height / 2) {
            return 0
        }
        if (y < height / 2) {
            return if (x < width / 2) 1 else 2
        }
        return if (x < width / 2) 3 else 4
    }
}

fun robotFromString(s: String): Robot {
    // Parse a line like "p=0,4 v=3,-3"
    val pattern = """p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)""".toRegex()
    val match = pattern.matchEntire(s) ?: throw IllegalArgumentException("Invalid input: $s")
    val (x, y, vx, vy) = match.destructured
    return Robot(x.toInt(), y.toInt(), vx.toInt(), vy.toInt())
}

fun makeRobotMap(robots: List<Robot>): String {
    // Count robots at each square.
    val counts = Array(height) { IntArray(width) }
    for (robot in robots) {
        counts[robot.y][robot.x]++
    }

    // Create a string representation of the map.
    return counts.joinToString("\n") { row ->
        row.joinToString("") {
            if (it == 0) "." else it.toString(36)
        }
    }
}

fun main(args: Array<String>) {
    val robots = generateSequence(::readLine).map { robotFromString(it) }.toList()

    // Print the first 5 steps.
    for (i in 0..<100) {
        robots.forEach { it.step() }
    }

    println(makeRobotMap(robots))

    robots.groupingBy { it.quadrant() }.eachCount().filter { it.key > 0 }.values.reduce(Int::times).let(::println)
}