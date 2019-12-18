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

  case class RValue(intcode: List[Int], output: List[Int])
  @annotation.tailrec
  def compute(
               intcode: List[Int],
               input: List[Int] = List(),
               pos: Int = 0,
               output: List[Int] = List()): RValue =
  {
    val opcodeAndParams = parseOpcode(intcode(pos))

    def getValue(i:Int) =
      (if (i < opcodeAndParams.length) opcodeAndParams(i) else 0) match {
        case 0 => intcode(intcode(pos + i)) // position mode
        case 1 => intcode(pos + i) // immediate mode
      }

    opcodeAndParams.head match {
      case 1 => // Add
        compute(
          intcode.updated(intcode(pos + 3), getValue(1) + getValue(2)),
          input,
          pos + 4,
          output)
      case 2 => // Mul
        compute(
          intcode.updated(intcode(pos + 3), getValue(1) * getValue(2)),
          input,
          pos + 4,
          output)
      case 3 => // Input
        if (input.nonEmpty)
          compute(
            intcode.updated(intcode(pos + 1), input.head),
            input.drop(1),
            pos + 2,
            output)
        else {
          println("Error: not enough input")
          compute(intcode, input, pos + 2, output)
        }
      case 4 => // Output
        compute(
          intcode,
          input,
          pos + 2,
          getValue(1) :: output)
      case 99 =>
        RValue(intcode, output)
    }
  }
}
