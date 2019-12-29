package AoC_2019

import scala.collection.immutable.Queue
import scala.language.implicitConversions

object Day5 {
  type Intcode = List[Long]
  case class Intprog(
                      code: Intcode,
                      pointer: Int = 0,
                      relativeBase: Int = 0,
                      input: Queue[Long] = Queue(),
                      output: List[Long] = List(),
                      finished: Boolean = false) {
    def process(): Intprog = compute(this)
    def |>(moreProg: Intprog): Intprogs = Intprogs(Queue(this, moreProg))
  }
  case object Intprog {
    def apply(code: List[Int]): Intprog = apply(code.map(i => i.toLong))
  }

  case class Intprogs(progs: Queue[Intprog]) {
    def |>(moreProg: Intprog): Intprogs = Intprogs(this.progs.enqueue(moreProg))
  }

  case class Intinput(data: Queue[Long]) {
    def |>(moreData: Int): Intinput = Intinput(data.enqueue(moreData.toLong))
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

  private def debugOutput(prog: Intprog): Unit = {
    val Intprog(intcode, intpointer, relativeBase, input, output, _) = prog
    val opcodeAndParams = parseOpcode(intcode(intpointer).toInt)

    def debugOut(name: String, count: Int = 0, txt: String = ""): Unit = {
        print(intpointer + " " + name + " " + count)
        if (count > 0)
          println("(" +
            (0 to count)
              .map(c => intcode(intpointer + c).toString)
              .reduce((c1, c2) => c1 + " " + c2)
            + ") " +
            txt)
        else println()
      }

    opcodeAndParams.head match {
      case 1 => // Add
        debugOut("Add ", 3)
      case 2 => // Mul
        debugOut("Mul", 3)
      case 3 => // Input
        if (input.nonEmpty) {
          debugOut("In", 1, input.head + " => ?") // + writeTo(1))
          //println("in: " + input.head)
        }
      case 4 => // Output
        debugOut("Out", 1)
        //debugOut("out: " + getValue(1) + " ... " + (getValue(1) :: output))
      case 5 => // jump-if-true
        debugOut("Jmp!=0 ", 2)
      case 6 => // jump-if-false
        debugOut("Jmp=0 ", 2)
      case 7 => // less than
        debugOut("Less", 3)
      case 8 => // equals
        debugOut("Equ", 3)
      case 9 => // adjust relative base
        debugOut("RB", 1, " rb(" + prog.relativeBase + ") + ?") // + getValue(1))
      case 99 =>
        debugOut("FIN")
        prog.copy(finished = true)
    }
  }

  @annotation.tailrec
  private def compute(prog: Intprog): Intprog =
  {
    val Intprog(intcode, intpointer, relativeBase, input, output, _) = prog
    val opcodeAndParams = parseOpcode(intcode(intpointer).toInt)

    //debugOutput(prog)

    def get(i: Int) = if (i >= intcode.size) 0L else intcode(i)

    def getRef(i: Int) = {
      (if (i < opcodeAndParams.length) opcodeAndParams(i) else 0) match {
        // position mode
        case 0 => get(intpointer + i).toInt
        // immediate mode
        case 1 => intpointer + i
        // relative mode
        case 2 => get(intpointer + i).toInt + relativeBase
      }
    }

    def getValue(i:Int) = get(getRef(i))

    def updated(i:Int, value:Long) =
      if (i < intcode.size)
        intcode.updated(i, value)
      else {
        val zeros = List.fill(i - intcode.size + 1)(0L)
        val newIntcode = intcode ::: zeros
        newIntcode.updated(i, value)
      }

    opcodeAndParams.head match {
      case 1 => // Add
        compute(prog.copy(
          code = updated(getRef(3), getValue(1) + getValue(2)),
          pointer = intpointer + 4))
      case 2 => // Mul
        compute(prog.copy(
          code = updated(getRef(3), getValue(1) * getValue(2)),
          pointer = intpointer + 4))
      case 3 => // Input
        if (input.nonEmpty) {
          compute(prog.copy(
            code = updated(getRef(1), input.head),
            pointer = intpointer + 2,
            input = input.drop(1)))
        }
        else prog
      case 4 => // Output
        compute(prog.copy(
          pointer = intpointer + 2,
          output = getValue(1) :: output))
      case 5 => // jump-if-true
        compute(prog.copy(
          pointer = if (getValue(1) != 0) getValue(2).toInt else intpointer + 3))
      case 6 => // jump-if-false
        compute(prog.copy(
          pointer = if (getValue(1) == 0) getValue(2).toInt else intpointer + 3))
      case 7 => // less than
        compute(prog.copy(
          code = updated(
            getRef(3),
            if (getValue(1) < getValue(2)) 1 else 0),
          pointer = intpointer + 4))
      case 8 => // equals
        compute(prog.copy(
          code = updated(
            getRef(3),
            if (getValue(1) == getValue(2)) 1 else 0),
          pointer = intpointer + 4))
      case 9 => // adjust relative base
        compute(prog.copy(
          pointer = intpointer + 2,
          relativeBase = prog.relativeBase + getValue(1).toInt
        ))
      case 99 =>
        prog.copy(finished = true)
    }
  }
}
