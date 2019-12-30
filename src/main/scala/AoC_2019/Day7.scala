package AoC_2019
import scala.collection.immutable.Queue
import Day5.{Intprog, Intprogs}

object Day7 {
  @annotation.tailrec
  private def computeProgs(progsIn: Queue[Intprog],
                           lastOutput: Option[List[Long]] = None,
                           progsOut: List[Intprog] = List())
  : List[Intprog] = {
    progsIn match {
      case in +: rest =>
        lastOutput match {
          case None =>
            val newProc = in.process()
            computeProgs(rest, Some(newProc.output), List(newProc))
          case Some(outputs) =>
            val newProc = in.copy(input = in.input.enqueueAll(outputs)).process()
            computeProgs(rest, Some(newProc.output), newProc :: progsOut)
        }
      case _ => progsOut
    }
  }

  def compute(progs: Intprogs): Long = {
    computeProgs(progs.progs).head.output.head
  }

  def computeFeedbackLoop(progs: Intprogs): Long = {
    @annotation.tailrec
    def cfl(progs: Queue[Intprog], lastOutput: Option[List[Long]] = None): List[Intprog] = {
      val result = computeProgs(progs, lastOutput)
      if (result.head.finished)
        result
      else {
        cfl(
          Queue.from(result.reverse.map(prog => prog.copy(output = List()))),
          Some(result.head.output))
      }
    }

    cfl(progs.progs).head.output.head
  }

  def amplifiers(code: List[Int], inputs: List[Int]): Day5.Intprogs =
  {
    import Day5.intinput

    val intprog1 = Intprog(code)
    (inputs.head |> 0 |> intprog1) |>
      (inputs(1) |> intprog1) |>
      (inputs(2) |> intprog1) |>
      (inputs(3) |> intprog1) |>
      (inputs(4) |> intprog1)
  }

  def findBestThrusterInput(intcode: List[Int]): Long = {
    val amps = amplifiers(intcode, _)
    (0 to 4).toList.permutations.map(input => compute(amps(input))).max
  }

  def findBestThrusterInputWithFeedbackLoop(intcode: List[Int]): Long = {
    val amps = amplifiers(intcode, _)
    (5 to 9).toList.permutations.map(input => computeFeedbackLoop(amps(input))).max
  }
}
