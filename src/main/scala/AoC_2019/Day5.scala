package AoC_2019

import scala.collection.immutable.Queue
import scala.language.implicitConversions

object Day5 {
  type Intcode = List[Int]
  case class Intprog(
                      code: Intcode,
                      pointer: Int = 0,
                      input: Queue[Int] = Queue(),
                      output: List[Int] = List(),
                      finished: Boolean = false) {
    def process(): Intprog = compute(this)
    def |>(moreProg: Intprog): Intprogs = Intprogs(Queue(this, moreProg))
  }

  case class Intprogs(progs: Queue[Intprog]) {
    def |>(moreProg: Intprog): Intprogs = Intprogs(this.progs.enqueue(moreProg))
  }

  case class Intinput(data: Queue[Int]) {
    def |>(moreData: Int): Intinput = Intinput(data.enqueue(moreData))
    def |>(program: Intprog): Intprog = program.copy(input = program.input.enqueueAll(data))
  }

  implicit def intinput(data: Int): Intinput = Intinput(Queue(data))

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

  @annotation.tailrec
  private def compute(prog: Intprog): Intprog =
  {
    val Intprog(intcode, intpointer, input, output, _) = prog
    val opcodeAndParams = parseOpcode(intcode(intpointer))

    def getValue(i:Int) =
      (if (i < opcodeAndParams.length) opcodeAndParams(i) else 0) match {
        case 0 => intcode(intcode(intpointer + i)) // position mode
        case 1 => intcode(intpointer + i) // immediate mode
      }
    def writeTo(i:Int) = intcode(intpointer + i)

    opcodeAndParams.head match {
      case 1 => // Add
        compute(prog.copy(
          code = intcode.updated(writeTo(3), getValue(1) + getValue(2)),
          pointer = intpointer + 4))
      case 2 => // Mul
        compute(prog.copy(
          code = intcode.updated(writeTo(3), getValue(1) * getValue(2)),
          pointer = intpointer + 4))
      case 3 => // Input
        if (input.nonEmpty) {
          //println("in: " + input.head)
          compute(prog.copy(
            code = intcode.updated(writeTo(1), input.head),
            pointer = intpointer + 2,
            input = input.drop(1)))
        }
        else prog
      case 4 => // Output
        //println("out: " + getValue(1) + " ... " + getValue(1) :: output)
        compute(prog.copy(
          pointer = intpointer + 2,
          output = getValue(1) :: output))
      case 5 => // jump-if-true
        compute(prog.copy(
          pointer = if (getValue(1) != 0) getValue(2) else intpointer + 3))
      case 6 => // jump-if-false
        compute(prog.copy(
          pointer = if (getValue(1) == 0) getValue(2) else intpointer + 3))
      case 7 => // less than
        compute(prog.copy(
          code = intcode.updated(
            writeTo(3),
            if (getValue(1) < getValue(2)) 1 else 0),
          pointer = intpointer + 4))
      case 8 => // equals
        compute(prog.copy(
          code = intcode.updated(
            writeTo(3),
            if (getValue(1) == getValue(2)) 1 else 0),
          pointer = intpointer + 4))
      case 99 =>
        //println("FIN")
        prog.copy(finished = true)
    }
  }
}
