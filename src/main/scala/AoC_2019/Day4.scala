package AoC_2019

object Day4 {
  @annotation.tailrec
  def countAdjacentMatches(digits: List[Int], lastDigit: Int, count: Int, counts: List[Int] = List()): List[Int] = {
    if (digits.isEmpty)
      count :: counts
    else {
      if (digits.head == lastDigit)
        countAdjacentMatches(digits.drop(1), lastDigit, count + 1, counts)
      else
        countAdjacentMatches(digits.drop(1), digits.head, 1, count :: counts)
    }
  }
  def countAdjacentMatches(digits: List[Int]): List[Int] = {
    countAdjacentMatches(digits.drop(1), digits(0), 1)
  }

  def checkPassword(password: Int, part1: Boolean = true) : Boolean = {
    val digits = password.toString.map((c: Char) => c.asDigit).toList
    val neighbours = digits.take(5) zip digits.drop(1)

    val is6digits = () => digits.size == 6
    val hasTwin = () =>
      if (part1) neighbours.exists{ case (d1, d2) => d1 == d2 }
      else countAdjacentMatches(digits).contains(2)
    val keepsIncreasing = () =>
      neighbours.forall{ case (d1, d2) => d2 >= d1 }

    is6digits() && hasTwin() && keepsIncreasing()
  }

  def validPassword(rangeBegin: Int, rangeEnd: Int, part1: Boolean = true) : List[Int] = {
    (rangeBegin to rangeEnd).filter(checkPassword(_, part1)).toList
  }
}
