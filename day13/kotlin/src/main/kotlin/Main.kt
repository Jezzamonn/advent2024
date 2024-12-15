data class Point(val x: Int, val y: Int) {
    operator fun plus(other: Point) = Point(x + other.x, y + other.y)
}

// Button A: X+94, Y+34
// Button B: X+22, Y+67
// Prize: X=8400, Y=5400

val pattern = Regex(
    """
    Button A: X\+(\d+), Y\+(\d+)
    Button B: X\+(\d+), Y\+(\d+)
    Prize: X=(\d+), Y=(\d+)
    """.trimIndent()
)
fun part1(input: String) {
    pattern.findAll(input).sumOf { match ->
        val (aX, aY, bX, bY, pX, pY) = match.destructured
        1.0
    }.let(::println)
}

fun part2(input: String) {
}

fun main(args: Array<String>) {
    if (args.isNotEmpty() && args.size == 2 && args[0] == "--part") {
        val input = generateSequence(::readLine).joinToString("\n")
        when (args[1]) {
            "1" -> part1(input)
            "2" -> part2(input)
            else -> println("Invalid part number. Use 1 or 2.")
        }
    } else {
        println("Usage: --part <1|2>")
    }
}