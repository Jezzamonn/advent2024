fun main(args: Array<String>) {
    val argsJoined = args.joinToString(" ")
    when {
        "--part 1" in argsJoined -> part1()
        "--part 2" in argsJoined -> part2()
        else -> throw IllegalArgumentException("Missing or invalid --part argument")
    }
}

val pattern = """
    Register A: (\d+)
    Register B: (\d+)
    Register C: (\d+)
    
    Program: (.+)
""".trimIndent()

enum class Instruction {
    ADV,
    BXL,
    BST,
    JNZ,
    BXC,
    OUT,
    BDV,
    CDV;

    companion object {
        fun fromInt(i: Int): Instruction {
            return entries[i]
        }
    }
}

fun part1() {
    val input = generateSequence (::readLine).joinToString("\n")
    val (aStr, bStr, cStr, programStr) = pattern.toRegex().matchEntire(input)!!.destructured

    var a = aStr.toInt()
    var b = bStr.toInt()
    var c = cStr.toInt()
    val program = programStr.split(",").map { it[0] - '0' }

    var instructionPointer = 0

    val out = mutableListOf<Int>()

    while (true) {
        if (instructionPointer !in program.indices) {
            break
        }
        val instruction = Instruction.fromInt(program[instructionPointer])
        val literalOperand = program[instructionPointer + 1]
        val comboOperand = when(literalOperand) {
            0, 1, 2, 3 -> literalOperand
            4 -> a
            5 -> b
            6 -> c
            else -> null
        }

        // Clear console
        print("\u001b[H\u001b[2J")
        println("""
            A: $a
            B: $b
            C: $c
            Instruction: $instruction
            Literal Operand: $literalOperand
            Combo Operand: $comboOperand
            Out: $out
        """.trimIndent())

        when (instruction) {
            Instruction.ADV -> {
                a = a / (1 shl comboOperand!!)
            }
            Instruction.BXL -> {
                b = b xor literalOperand
            }
            Instruction.BST -> {
                b = comboOperand!! and 0b111
            }
            Instruction.JNZ -> {
                if (a != 0) {
                    instructionPointer = literalOperand - 2
                }
            }
            Instruction.BXC -> {
                b = b xor c
            }
            Instruction.OUT -> {
                out.add(comboOperand!! and 0b111)
            }
            Instruction.BDV -> {
                b = a / (1 shl comboOperand!!)
            }
            Instruction.CDV -> {
                c = a / (1 shl comboOperand!!)
            }
        }

        instructionPointer += 2
    }

    println(out.joinToString(","))
}

fun part2() {}
