import AoC_2019.Day10.Asteroid
import AoC_2019.Day5.{Intinput, Intprog}
import AoC_2019.Day8.{Image, Layer, Row}
import AoC_2019.{Day1, Day10, Day2, Day3, Day4, Day5, Day6, Day7, Day8}
import org.scalatest.FunSuite

class AoC_2019_Test extends FunSuite {
  // DAY 1
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

  // DAY 2
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

  // DAY 3
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

  // DAY 4
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

  // DAY 5
  test("AoC_2019.Day5 Part1") {
    import Day5.intinput

    val prog1 = (99 |> Intprog(List(3,0,4,0,99))).process()
    assert(prog1 === Intprog(List(99,0,4,0,99), 4, output = List(99), finished = true))

    val prog2 = Intprog(List(1002,4,3,4,33)).process()
    assert(prog2 === Intprog(List(1002,4,3,4,99), 4, finished = true))
  }
  test("AoC_2019.Day5 Part2") {
    import Day5.intinput

    val prog1 = Intprog(List(3,9,8,9,10,9,4,9,99,-1,8))
    assert((99 |> prog1).process().output(0) === 0)
    assert((8 |> prog1).process().output(0) === 1)
    assert((1 |> prog1).process().output(0) === 0)

    val prog2 = Intprog(List(3,9,7,9,10,9,4,9,99,-1,8))
    assert((99 |> prog2).process().output(0) === 0)
    assert((8 |> prog2).process().output(0) === 0)
    assert((1 |> prog2).process().output(0) === 1)

    val prog3 = Intprog(List(3,3,1108,-1,8,3,4,3,99))
    assert((99 |> prog3).process().output(0) === 0)
    assert((8 |> prog3).process().output(0) === 1)
    assert((1 |> prog3).process().output(0) === 0)

    val prog4 = Intprog(List(3,3,1107,-1,8,3,4,3,99))
    assert((99 |> prog4).process().output(0) === 0)
    assert((8 |> prog4).process().output(0) === 0)
    assert((1 |> prog4).process().output(0) === 1)

    val prog5 = Intprog(List(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9))
    assert((-1 |> prog5).process().output(0) === 1)
    assert((0 |> prog5).process().output(0) === 0)
    assert((1 |> prog5).process().output(0) === 1)

    val prog6 = Intprog(List(3,3,1105,-1,9,1101,0,0,12,4,12,99,1))
    assert((-1 |> prog6).process().output(0) === 1)
    assert((0 |> prog6).process().output(0) === 0)
    assert((1 |> prog6).process().output(0) === 1)

    val prog7 = Intprog(List(3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
      1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
      999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99))
    assert((0 |> prog7).process().output.head === 999)
    assert((8 |> prog7).process().output.head === 1000)
    assert((100 |> prog7).process().output.head === 1001)
  }
  test("AoC_2019.Day5 Results") {
    import Day5.intinput

    val source = scala.io.Source.fromFile("resources\\day5_input.txt")
    val lines = source.getLines().toArray
    source.close()

    val code = lines(0).split(',').map(s => s.toInt).toList
    val intcode = Intprog(code)
    assert((1 |> intcode).process().output === List(7566643, 0, 0, 0, 0, 0, 0, 0, 0, 0))
    assert((5 |> intcode).process().output.head === 9265694)
  }

  // DAY 6
  test("AoC_2019.Day6 Part1") {
    assert(Day6.calcTotalNumberOfOrbits("COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L") === 42)
  }
  test("AoC_2019.Day6 Part2") {
    val orbits = "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN"
    assert(Day6.calcDistance(orbits, "YOU", "SAN") === 4)
  }

  // DAY 7
  test("AoC_2019.Day7 Part1") {
    val test1 = Day7.amplifiers(
      List(3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0),
      List(4,3,2,1,0))
    assert(Day7.compute(test1) === 43210)

    val test2 = Day7.amplifiers(
      List(3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0),
      List(0,1,2,3,4))
    assert(Day7.compute(test2) === 54321)

    val test3 = Day7.amplifiers(
      List(3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0),
      List(1,0,4,3,2)
    )
    assert(Day7.compute(test3) === 65210)
  }
  test("AoC_2019.Day7 Part2") {
    val test1 = Day7.amplifiers(
      List(3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5),
      List(9,8,7,6,5))
    assert(Day7.computeFeedbackLoop(test1) === 139629729)

    val test2 = Day7.amplifiers(
      List(3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5),
      List(9,8,7,6,5)
    )
    assert(Day7.computeFeedbackLoop(test2) === 139629729)

    val test3 = Day7.amplifiers(
      List(3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
        -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
        53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10),
      List(9,7,8,5,6)
    )
    assert(Day7.computeFeedbackLoop(test3) === 18216)
  }
  test("AoC_2019.Day7 Results") {
    val source = scala.io.Source.fromFile("resources\\day7_input.txt")
    val lines = source.getLines().toArray
    source.close()

    val intcode = lines(0).split(',').map(s => s.toInt).toList

    assert(Day7.findBestThrusterInput(intcode) === 20413)
    assert(Day7.findBestThrusterInputWithFeedbackLoop(intcode) === 3321777)
  }

  // DAY 8
  test("AoC_2019.Day8 Part2") {
    val strImage = "0222112222120000"
    val img = Day8.stringToImage(strImage, 2, 2)
    assert(Day8.renderImage(img, false) === Image(List(Layer(List(Row(List(0, 1)), Row(List(1, 0))))), 2, 2))
  }

  // DAY 9
  test("AoC_2019.Day9") {
    val intcode1 = List(109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99)
    assert(Intprog(intcode1).process().output === intcode1.reverse)

    val intcode2 = List(1102,34915192,34915192,7,4,7,99,0)
    assert(Intprog(intcode2).process().output.head.toString.length === 16)
    assert(Intprog(intcode2).process().output.head === 1219070632396864L)

    val intcode3 = List(104,1125899906842624L,99)
    assert(Intprog(intcode3).process().output.head === 1125899906842624L)
  }

  test("AoC_2019.Day9 Results") {
    import Day5.intinput

    val source = scala.io.Source.fromFile("resources\\day9_input.txt")
    val lines = source.getLines().toArray
    source.close()

    val intcode = lines(0).split(',').map(s => s.toLong).toList
    assert((1 |> Intprog(intcode)).process().output.head === 3512778005L)
    //assert((2 |> Intprog(intcode)).process().output.head === 35920)
  }

  // DAY 10
  test("AoC_2019.Day10") {
    val input1 = ".#..#\n.....\n#####\n....#\n...##"
    assert(Day10.computeBestLocation(input1) === (Asteroid(3,4), 8))

    val input2 = "......#.#.\n#..#.#....\n..#######.\n.#.#.###..\n.#..#.....\n..#....#.#\n#..#....#.\n.##.#..###\n##...#..#.\n.#....####"
    assert(Day10.computeBestLocation(input2) === (Asteroid(5,8), 33))

    val input3 = "#.#...#.#.\n.###....#.\n.#....#...\n##.#.#.#.#\n....#.#.#.\n.##..###.#\n..#...##..\n..##....##\n......#...\n.####.###."
    assert(Day10.computeBestLocation(input3) === (Asteroid(1,2), 35))

    val input4 = ".#..#..###\n####.###.#\n....###.#.\n..###.##.#\n##.##.#.#.\n....###..#\n..#.#..#.#\n#..#.#.###\n.##...##.#\n.....#.#.."
    assert(Day10.computeBestLocation(input4) === (Asteroid(6,3), 41))

    val input5 = ".#..##.###...#######\n##.############..##.\n.#.######.########.#\n.###.#######.####.#.\n#####.##.#.##.###.##\n..#####..#.#########\n####################\n#.####....###.#.#.##\n##.#################\n#####.##.###..####..\n..######..##.#######\n####.##.####...##..#\n.#####..#.######.###\n##...#.##########...\n#.##########.#######\n.####.#.###.###.#.##\n....##.##.###..#####\n.#.#.###########.###\n#.#.#.#####.####.###\n###.##.####.##.#..##"
    assert(Day10.computeBestLocation(input5) === (Asteroid(11,13), 210))
  }
}
