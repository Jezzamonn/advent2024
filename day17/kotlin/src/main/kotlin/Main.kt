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

class ProgramState(var a: Int, var b: Int, var c: Int, var instructionPointer: Int, val program: List<Int>, val out: MutableList<Int>) {

    fun newProgram(): ProgramState {
        return ProgramState(a, b, c, 0, program, mutableListOf())
    }

    companion object {
        fun fromInput(input: String): ProgramState {
            val (aStr, bStr, cStr, programStr) = pattern.toRegex().matchEntire(input)!!.destructured

            val a = aStr.toInt()
            val b = bStr.toInt()
            val c = cStr.toInt()
            val program = programStr.split(",").map { it[0] - '0' }

            return ProgramState(a, b, c, 0, program, mutableListOf())
        }
    }

    fun step(): Boolean {
        if (instructionPointer !in program.indices) {
            return false
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

//        // Clear console
//        print("\u001b[H\u001b[2J")
//        println("""
//            A: $a
//            B: $b
//            C: $c
//            Instruction: $instruction
//            Literal Operand: $literalOperand
//            Combo Operand: $comboOperand
//            Out: $out
//        """.trimIndent())

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

        return true
    }
}

fun part1() {
    val input = generateSequence (::readLine).joinToString("\n")

    val state = ProgramState.fromInput(input)

    while (state.step()) {}

    println(state.out.joinToString(","))
}

fun part2() {
    val input = generateSequence (::readLine).joinToString("\n")

    val baseState = ProgramState.fromInput(input)

    var i = 0
    while (true) {
        val state = baseState.newProgram()
        state.a = i

        if (i % 1000000 == 0) {
            println(i)
        }

        var lastOutLen = 0

        while (state.step()) {
            // Early terminations:
            // If out is too big:
            if (state.out.size > baseState.program.size) {
                break
            }

            if (state.out.size > lastOutLen) {
                lastOutLen = state.out.size

                // If out doesn't match the start of the program:
                for ((outIndex, out) in state.out.withIndex()) {
                    if (baseState.program[outIndex] != out) {
                        break
                    }
                }
            }
        }

        // Check if state.out = baseState.program
        if (state.out == baseState.program) {
            println(i)
            return
        }


//            if (state.out.size > baseState.program.size) {
//                break
//            }
//
//            for ((outIndex, out) in state.out.withIndex()) {
//                if (baseState.program[outIndex] != out) {
//                    break
//                }
//            }
//
//            if (state.out.size == baseState.program.size) {
//                println(i)
//                return
//            }
//        }
        i++
    }

}
