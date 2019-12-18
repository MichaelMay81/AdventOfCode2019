import AoC_2019.{Day1, Day2, Day3, Day4, Day5, Day6}
import org.scalatest.FunSuite

class AoC_2019_Test extends FunSuite {
  test("AoC_2019.Day1_Part1") {
    assert(Day1.fuel_required(12) === 2)
    assert(Day1.fuel_required(14) === 2)
    assert(Day1.fuel_required(1969) === 654)
    assert(Day1.fuel_required(100756) === 33583)
  }
  test("AoC_2019.Day1_Part2") {
    assert(Day1.total_fuel_required(14) === 2)
    assert(Day1.total_fuel_required(1969) === 966)
    assert(Day1.total_fuel_required(100756) === 50346)
  }
  test("AoC_2019.Day2") {
    val data = List(
      (List(1,0,0,0,99), List(2,0,0,0,99)),
      (List(2,3,0,3,99), List(2,3,0,6,99)),
      (List(2,4,4,5,99,0), List(2,4,4,5,99,9801)),
      (List(1,1,1,4,99,5,6,0,99), List(30,1,1,4,2,5,6,0,99))
    )
    for ((in, out) <- data)
      assert(Day2.compute(in) === out)
  }
  test("AoC_2019.Day3 Part1") {
    val data = List(
      ("R8,U5,L5,D3", "U7,R6,D4,L4", 6),
      ("R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83", 159),
      ("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7", 135)
    )
    for ((in1, in2, out) <- data)
      assert(Day3.calcDistanceToClosestIntersection(in1, in2) === out)
  }
  test("AoC_2019.Day3 Part2") {
    val data = List(
      ("R8,U5,L5,D3", "U7,R6,D4,L4", 30),
      ("R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83", 610),
      ("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7", 410)
    )
    for ((in1, in2, out) <- data)
      assert(Day3.calcStepsToClosestIntersection(in1, in2) === out)
  }
  test("AoC_2019.Day4 Part1") {
    assert(Day4.checkPassword(111111))
    assert(! Day4.checkPassword(223450))
    assert(! Day4.checkPassword(123789))
  }
  test("AoC_2019.Day4 Part2") {
    assert(Day4.checkPassword(112233, false))
    assert(! Day4.checkPassword(123444, false))
    assert(Day4.checkPassword(111122, false))
  }
  test("AoC_2019.Day5 Part1") {
    assert(Day5.compute(List(3,0,4,0,99), List(99)) === Day5.RValue(List(99,0,4,0,99), List(99)))
    assert(Day5.compute(List(1002,4,3,4,33)) === Day5.RValue(List(1002,4,3,4,99), List()))
  }
  test("AoC_2019.Day6 Part1") {
    assert(Day6.calcTotalNumberOfOrbits("COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L") === 42)
  }
}
