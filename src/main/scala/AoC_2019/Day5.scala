package AoC_2019

object Day5 {
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

  case class RValue(intcode: List[Int], output: List[Int], intpointer: Option[Int])
  @annotation.tailrec
  def compute(
               intcode: List[Int],
               input: List[Int] = List(),
               intpointer: Int = 0,
               output: List[Int] = List()): RValue =
  {
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
          intcode.updated(writeTo(3), getValue(1) + getValue(2)),
          input,
          intpointer + 4,
          output)
      case 2 => // Mul
        compute(
          intcode.updated(writeTo(3), getValue(1) * getValue(2)),
          input,
          intpointer + 4,
          output)
      case 3 => // Input
        if (input.nonEmpty)
          compute(
            intcode.updated(writeTo(1), input.head),
            input.drop(1),
            intpointer + 2,
            output)
        else {
          RValue(intcode, output, Some(intpointer))
        }
      case 4 => // Output
        compute(
          intcode,
          input,
          intpointer + 2,
          getValue(1) :: output)
      case 5 => // jump-if-true
        val newIntPointer = if (getValue(1) != 0) getValue(2) else intpointer + 3
        compute(intcode, input, newIntPointer, output)
      case 6 => // jump-if-false
        val newIntPointer = if (getValue(1) == 0) getValue(2) else intpointer + 3
        compute(intcode, input, newIntPointer, output)
      case 7 => // less than
        compute(
          intcode.updated(
            writeTo(3),
            if (getValue(1) < getValue(2)) 1 else 0),
          input, intpointer + 4, output)
      case 8 => // equals
        compute(
          intcode.updated(
            writeTo(3),
            if (getValue(1) == getValue(2)) 1 else 0),
          input, intpointer + 4, output)
      case 99 =>
        RValue(intcode, output, None)
    }
  }
}
