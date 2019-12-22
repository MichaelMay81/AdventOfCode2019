package AoC_2019

object Day5 {
  type Intcode = List[Int]
  case class Intprog(code: Intcode, pointer: Int = 0, finished: Boolean = false)

  private def parseOpcode(opcode: Int) : List[Int] = {
    if (opcode < 100)
      List(opcode)
    else {
      val str = opcode.toString
      val ocIndex = str.length - 2

      val oc = str.substring(ocIndex).toInt
      val modes = str
        .substring(0, ocIndex)
        .map((c: Char) => c.asDigit)
        .toList
        .reverse

      oc :: modes
    }
  }

  case class RValue(program: Intprog, output: List[Int])
  @annotation.tailrec
  def compute(program: Intprog,
               input: List[Int] = List(),
               output: List[Int] = List()): RValue =
  {
    val intcode = program.code
    val intpointer = program.pointer
    val opcodeAndParams = parseOpcode(intcode(intpointer))

    def getValue(i:Int) =
      (if (i < opcodeAndParams.length) opcodeAndParams(i) else 0) match {
        case 0 => intcode(intcode(intpointer + i)) // position mode
        case 1 => intcode(intpointer + i) // immediate mode
      }
    def writeTo(i:Int) = intcode(intpointer + i)

    opcodeAndParams.head match {
      case 1 => // Add
        compute(
          Intprog(
            intcode.updated(writeTo(3), getValue(1) + getValue(2)),
            intpointer + 4),
          input,
          output)
      case 2 => // Mul
        compute(
          Intprog(
            intcode.updated(writeTo(3), getValue(1) * getValue(2)),
            intpointer + 4),
          input,
          output)
      case 3 => // Input
        if (input.nonEmpty)
          compute(
            Intprog(
              intcode.updated(writeTo(1), input.head),
              intpointer + 2),
            input.drop(1),
            output)
        else {
          RValue(program, output)
        }
      case 4 => // Output
        compute(
          Intprog(intcode, intpointer + 2),
          input,
          getValue(1) :: output)
      case 5 => // jump-if-true
        val newIntPointer = if (getValue(1) != 0) getValue(2) else intpointer + 3
        compute(Intprog(intcode, newIntPointer), input, output)
      case 6 => // jump-if-false
        val newIntPointer = if (getValue(1) == 0) getValue(2) else intpointer + 3
        compute(Intprog(intcode, newIntPointer), input, output)
      case 7 => // less than
        compute(
          Intprog(
            intcode.updated(
              writeTo(3),
              if (getValue(1) < getValue(2)) 1 else 0),
            intpointer + 4),
          input, output)
      case 8 => // equals
        compute(
          Intprog(
            intcode.updated(
              writeTo(3),
              if (getValue(1) == getValue(2)) 1 else 0),
            intpointer + 4),
          input, output)
      case 99 =>
        RValue(Intprog(program.code, program.pointer, finished = true), output)
    }
  }
}
