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

    fun position(): Pair<Int, Int> {
        return Pair(x, y)
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

    fun nearTopCorners(): Boolean {
        val distToTLCorner = x + y
        val distToTRCorner = (width - 1) - x + y
        return (distToTLCorner < width / 2) || (distToTRCorner < width / 2)
    }

    fun reflectedPosition(): Pair<Int, Int> {
        if (x > width / 2) {
            return Pair(width - x, y)
        }
        return Pair(x, y)
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
    val argsJoined = args.joinToString(" ")
    when {
        "--part 1" in argsJoined -> part1()
        "--part 2" in argsJoined -> part2()
        else -> throw IllegalArgumentException("Missing or invalid --part argument")
    }
}

fun part1() {
    val robots = generateSequence(::readLine).map { robotFromString(it) }.toList()

    // Print the first 5 steps.
    for (i in 0..<100) {
        robots.forEach { it.step() }
    }

    println(makeRobotMap(robots))

    robots.groupingBy { it.quadrant() }.eachCount().filter { it.key > 0 }.values.reduce(Int::times).let(::println)
}

fun part2() {
    val robots = generateSequence(::readLine).map { robotFromString(it) }.toList()

    println(makeRobotMap(robots))
    println()
    println()

    var bestScore = 0
    for (i in 0..<width * height) {

        val score = robots.map { it.position() }.toSet().size
        if (score > bestScore) {
            bestScore = score
            println(makeRobotMap(robots))
            println("Step $i: $score")
            println()
            println()
        }

        robots.forEach { it.step() }
    }
    // 6511: too low

//    var bestScore = Int.MAX_VALUE
//    for (i in 0..<width * height) {
//        robots.forEach { it.step() }
//
//        val score = robots.filter { it.nearTopCorners() }.size
//        if (score < bestScore) {
//            bestScore = score
//            println(makeRobotMap(robots))
//            println("Step $i: $score")
//            println()
//            println()
//        }
//    }
}

