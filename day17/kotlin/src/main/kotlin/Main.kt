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

class ProgramState(
    var a: Long,
    var b: Long,
    var c: Long,
    var instructionPointer: Int,
    val program: List<Int>,
    val out: MutableList<Int>
) {

    fun newProgram(): ProgramState {
        return ProgramState(a, b, c, 0, program, mutableListOf())
    }

    companion object {
        fun fromInput(input: String): ProgramState {
            val (aStr, bStr, cStr, programStr) = pattern.toRegex().matchEntire(input)!!.destructured

            val a = aStr.toLong()
            val b = bStr.toLong()
            val c = cStr.toLong()
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
        val comboOperand: Int? = when (literalOperand) {
            0, 1, 2, 3 -> literalOperand
            4 -> a.toInt()
            5 -> b.toInt()
            6 -> c.toInt()
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
                a = a / (1L shl comboOperand!!)
            }

            Instruction.BXL -> {
                b = b xor literalOperand.toLong()
            }

            Instruction.BST -> {
                b = (comboOperand!! and 0b111).toLong()
            }

            Instruction.JNZ -> {
                if (a != 0L) {
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
    val input = generateSequence(::readLine).joinToString("\n")

    val state = ProgramState.fromInput(input)

    while (state.step()) {
    }

    println(state.out.joinToString(","))
}

fun part2() {
    val input = generateSequence(::readLine).joinToString("\n")

    val baseState = ProgramState.fromInput(input)

    var runningAValue = 0L;
    for (programChar in baseState.program.reversed()) {
        for (i in 0..<(1 shl 3)) {
            val state = baseState.newProgram()
            state.a = (runningAValue shl 3) or i.toLong()
            while (state.step()) {}

            val firstOut = state.out[0]
            if (firstOut == programChar) {
                runningAValue = (runningAValue shl 3) or i.toLong()
                println(state.out)
                break
            }
        }
    }

    println("A value = $runningAValue")

    // Test that it works?
    val state = baseState.newProgram()
    state.a = runningAValue
    while (state.step()) {}

    println("Program = ${state.program}")
    println("Out     = ${state.out}")
}
