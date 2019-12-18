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
  test("AoC_2019.Day5 Part2") {
    assert(Day5.compute(List(3,9,8,9,10,9,4,9,99,-1,8), List(99)).output(0) === 0)
    assert(Day5.compute(List(3,9,8,9,10,9,4,9,99,-1,8), List(8)).output(0) === 1)
    assert(Day5.compute(List(3,9,8,9,10,9,4,9,99,-1,8), List(1)).output(0) === 0)

    assert(Day5.compute(List(3,9,7,9,10,9,4,9,99,-1,8), List(99)).output(0) === 0)
    assert(Day5.compute(List(3,9,7,9,10,9,4,9,99,-1,8), List(8)).output(0) === 0)
    assert(Day5.compute(List(3,9,7,9,10,9,4,9,99,-1,8), List(1)).output(0) === 1)

    assert(Day5.compute(List(3,3,1108,-1,8,3,4,3,99), List(99)).output(0) === 0)
    assert(Day5.compute(List(3,3,1108,-1,8,3,4,3,99), List(8)).output(0) === 1)
    assert(Day5.compute(List(3,3,1108,-1,8,3,4,3,99), List(1)).output(0) === 0)

    assert(Day5.compute(List(3,3,1107,-1,8,3,4,3,99), List(99)).output(0) === 0)
    assert(Day5.compute(List(3,3,1107,-1,8,3,4,3,99), List(8)).output(0) === 0)
    assert(Day5.compute(List(3,3,1107,-1,8,3,4,3,99), List(1)).output(0) === 1)

    assert(Day5.compute(List(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9), List(-1)).output(0) === 1)
    assert(Day5.compute(List(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9), List(0)).output(0) === 0)
    assert(Day5.compute(List(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9), List(1)).output(0) === 1)

    assert(Day5.compute(List(3,3,1105,-1,9,1101,0,0,12,4,12,99,1), List(-1)).output(0) === 1)
    assert(Day5.compute(List(3,3,1105,-1,9,1101,0,0,12,4,12,99,1), List(0)).output(0) === 0)
    assert(Day5.compute(List(3,3,1105,-1,9,1101,0,0,12,4,12,99,1), List(1)).output(0) === 1)

    val input = List(3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
      1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
      999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99)
    assert(Day5.compute(input, List(0)).output.head === 999)
    assert(Day5.compute(input, List(8)).output.head === 1000)
    assert(Day5.compute(input, List(100)).output.head === 1001)
  }

  test("AoC_2019.Day6 Part1") {
    assert(Day6.calcTotalNumberOfOrbits("COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L") === 42)
  }
}
