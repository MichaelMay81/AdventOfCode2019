package AoC_2019

object Day2 {
  private def process(intcode: List[Int], pos: Int, fun: (Int, Int) => Int) : List[Int] = {
    val num1 = intcode(intcode(pos + 1))
    val num2 = intcode(intcode(pos + 2))
    intcode.updated(intcode(pos + 3), fun(num1, num2))
  }

  @annotation.tailrec
  def compute(intcode: List[Int], pos: Int = 0) : List[Int] = {
    intcode(pos) match {
      case 1 =>
        compute(
          process(intcode, pos, (num1, num2) => num1 + num2),
          pos + 4)
      case 2 =>
        compute(
          process(intcode, pos, (num1, num2) => num1 * num2),
          pos + 4)
      case 99 =>
        intcode
    }
  }

  def compute(intcode: List[Int], noun: Int, verb: Int) : Int = {
    val intcodeWNaV = intcode.updated(1, noun).updated(2, verb)
    val intcodeResult = compute(intcodeWNaV)
    intcodeResult.head
  }

  def findNounAndVerb(intcode: List[Int], expectedOutput: Int) : Option[(Int, Int)] = {
    val results = (0 to 99)
      .flatMap(noun => (0 to 99)
        .map(verb => (noun, verb, compute(intcode, noun, verb))))

    results
      .find(_._3 == expectedOutput)
      .map { case (noun, value, _) => (noun, value) }
  }
}
