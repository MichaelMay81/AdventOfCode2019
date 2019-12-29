package AoC_2019

import AoC_2019.Day5.Intprog

object Main {
  def main(args: Array[String]): Unit = {
    println("Hello world!")
    day9()
  }

  def day1(): Unit = {
    val source = scala.io.Source.fromFile("resources\\day1_input.txt")
    val lines = source.getLines().toArray
    source.close()

    val masses = lines.map(s => s.toInt)
    val fuel = masses.map(mass => Day1.fuel_required(mass))
    println("day1 part1 result: " + fuel.sum)

    val total_fuel = masses.map(mass => Day1.total_fuel_required(mass))
    println("day1 part2 result: " + total_fuel.sum)
  }

  def day2(): Unit = {
    val source = scala.io.Source.fromFile("resources\\day2_input.txt")
    val lines = source.getLines().toArray
    source.close()

    val intcode = lines(0).split(',').map(s => s.toInt).toList

    val output = Day2.compute(intcode, 12, 2)
    println("day2 part1 result: " + output)

    Day2.findNounAndVerb(intcode, 19690720) match {
      case None => println("day2 part2 ERROR, no result found")
      case Some((noun, verb)) => println("day2 part2 result: " + noun + " " + verb)
    }
  }

  def day3(): Unit = {
    val source = scala.io.Source.fromFile("resources\\day3_input.txt")
    val lines = source.getLines().toArray
    source.close()

    val path1 = lines(0)
    val path2 = lines(1)

    val result = Day3.calcDistanceToClosestIntersection(path1, path2)
    println("day3 part1 result: " + result)

    val result2 = Day3.calcStepsToClosestIntersection(path1, path2)
    println("day3 part2 result: " + result2)
  }

  def day4(): Unit = {
    println("day4 part1 result: " + Day4.validPassword(178416, 676461).size)
    println("day4 part1 result: " + Day4.validPassword(178416, 676461, false).size)
  }

  def day5(): Unit = {
    import Day5.intinput

    val source = scala.io.Source.fromFile("resources\\day5_input.txt")
    val lines = source.getLines().toArray
    source.close()

    val intcode = lines(0).split(',').map(s => s.toInt).toList

    val prog1 = 1 |> Intprog(intcode)
    val prog2 = 5 |> Intprog(intcode)
    println("day5 part1 result: " + prog1.process().output)
    println("day5 part2 result: " + prog2.process().output)
  }

  def day6(): Unit = {
    val source = scala.io.Source.fromFile("resources\\day6_input.txt")
    val input = source.mkString
    source.close()

    println("day6 part1 result: " + Day6.calcTotalNumberOfOrbits(input))
    println("day6 part2 result: " + Day6.calcDistance(input, "YOU", "SAN"))
  }

  def day7(): Unit = {
    val source = scala.io.Source.fromFile("resources\\day7_input.txt")
    val lines = source.getLines().toArray
    source.close()

    val intcode = lines(0).split(',').map(s => s.toInt).toList
    println("day7 part1 result: " + Day7.findBestThrusterInput(intcode))
    println("day7 part2 result: " + Day7.findBestThrusterInputWithFeedbackLoop(intcode))
  }

  def day8(): Unit = {
    val source = scala.io.Source.fromFile("resources\\day8_input.txt")
    val lines = source.getLines().toArray
    source.close()

    val input = Day8.stringToImage(lines(0), 25, 6)
    println("day8 part1 result: " + Day8.calcCheckSum(input))

    Day8.renderImage(input)
  }

  def day9(): Unit = {
    import Day5.intinput

    val source = scala.io.Source.fromFile("resources\\day9_input.txt")
    val lines = source.getLines().toArray
    source.close()

    val intcode = lines(0).split(',').map(s => s.toLong).toList
    println("day9 part1 result: " + (1 |> Intprog(intcode)).process().output)
    //println("day9 part2 result: " + (2 |> Intprog(intcode)).process().output)
  }
}
