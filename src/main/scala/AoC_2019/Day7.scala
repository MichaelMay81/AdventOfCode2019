package AoC_2019
import scala.collection.immutable.Queue

object Day7 {
  def calcThrusterOutput(intcode: List[Int], input: List[Int]): Int = {
    @annotation.tailrec
    def calcAmps(input: List[Int], lastOutput: Int = 0, intcodes: List[List[Int]] = List()): (Int, List[List[Int]]) =
      input match {
        case Nil => (lastOutput, intcodes)
        case in :: rest =>
          val result = Day5.compute(intcode, List(in, lastOutput))
          calcAmps(
            rest,
            result.output.head,
            result.intcode :: intcodes)
      }

    calcAmps(input)._1
  }
  def calcThrusterOutput2(intcode: List[Int], input: List[Int]): Int = {

    case class Status(input: Option[Int], intcode: List[Int] = intcode, pntr: Int = 0)

    @annotation.tailrec
    def calcAmps(processQueue: Queue[Status], lastOutput: List[Int] = List(0)): List[Int] =
      processQueue match {
        case in +: rest => {
          val input = in.input match {
            case None => lastOutput.reverse
            case Some(in) => in :: lastOutput.reverse
          }
          val result = Day5.compute(in.intcode, input, in.pntr)

          result.intpointer match {
            case None =>
              calcAmps(
                rest,
                result.output)
            case Some(pntr) =>
              calcAmps(
                rest.enqueue(Status(None, result.intcode, pntr)),
                result.output)
           }
        }
        case _ => lastOutput
      }

    calcAmps(Queue.from(input.map(i => Status(Some(i))))).sum
  }

  def findBestThrusterInput(intcode: List[Int]): Int = {
    (0 to 4).permutations.map(input => calcThrusterOutput(intcode, input.toList)).max
  }

  def findBestThrusterInput2(intcode: List[Int]): Int = {
    (5 to 9).permutations.map(input => calcThrusterOutput2(intcode, input.toList)).max
  }
}