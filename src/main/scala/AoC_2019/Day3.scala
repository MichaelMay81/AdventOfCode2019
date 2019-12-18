package AoC_2019

object Day3 {
  case class Position(x: Int, y: Int)

  private sealed trait Line
  private case class HorLine(x1: Int, x2: Int, y: Int) extends Line
  private case class VerLine(x: Int, y1: Int, y2: Int) extends Line

  @annotation.tailrec
  private def path2Line(paths: Array[String], lastPos: Position = Position(0, 0), lines: List[Line] = List()) : List[Line] = {
    if (paths.nonEmpty) {
      val cmd = paths.head.head
      val steps = paths.head.substring(1).toInt

      val newPos = cmd match {
        case 'U' => Position(lastPos.x, lastPos.y + steps)
        case 'D' => Position(lastPos.x, lastPos.y - steps)
        case 'R' => Position(lastPos.x + steps, lastPos.y)
        case 'L' => Position(lastPos.x - steps, lastPos.y)
      }

      val line = cmd match {
        case 'U' | 'D' => VerLine(lastPos.x, lastPos.y, newPos.y)
        case 'R' | 'L' => HorLine(lastPos.x, newPos.x, lastPos.y)
      }

      path2Line(paths.drop(1), newPos, line :: lines)
    }
    else lines
  }

  private def calcIntersection(hLine: HorLine, vLine: VerLine) : Option[Position] = {
    val xs = List(hLine.x1, hLine.x2).sorted
    val ys = List(vLine.y1, vLine.y2).sorted
    if (xs(0) <= vLine.x && vLine.x <= xs(1) && ys(0) <= hLine.y && hLine.y <= ys(1))
      Some(Position(vLine.x, hLine.y))
    else
      None
  }

  private def calcIntersections(line1: List[Line], line2: List[Line]) : List[(Position, Line, Line)] = {
    val line1hor = line1.collect { case h : HorLine => h }
    val line1ver = line1.collect { case h : VerLine => h }
    val line2hor = line2.collect { case h : HorLine => h }
    val line2ver = line2.collect { case h : VerLine => h }

    (line1hor.flatMap(l1h => line2ver.map(l2v => (calcIntersection(l1h, l2v), l1h, l2v))) :::
      line2hor.flatMap(l2h => line1ver.map(l1v => (calcIntersection(l2h, l1v), l1v, l2h))))
      .collect { case (Some(pos), l1, l2) => (pos, l1, l2) }
  }

  def calcDistanceToClosestIntersection(path1: String, path2: String) : Int = {
    val line1 = path2Line(path1.split(","))
    val line2 = path2Line(path2.split(","))

    val manhattan =
      calcIntersections(line1, line2)
        .map { case (pos, _, _) => math.abs(pos.x) + math.abs(pos.y) }
        .sorted

    if (manhattan.nonEmpty && manhattan(0) != 0)
      manhattan(0)
    else if (manhattan.length > 1)
      manhattan(1)
    else 0
  }

  private def calcDistance(line: List[Line], intersectionPoint: Position, intersectedLine: Line) : Int = {
    val index = line.indexOf(intersectedLine)
    val newLine = line.take(index)
    val distances = newLine.map {
      case line: HorLine => (line.x2 - line.x1).abs
      case line: VerLine => (line.y2 - line.y1).abs
    }

    val lastMile = line(index) match {
      case line: HorLine => (line.x1 - intersectionPoint.x).abs
      case line: VerLine => (line.y1 - intersectionPoint.y).abs
    }

    distances.sum + lastMile
  }

  def calcStepsToClosestIntersection(path1: String, path2: String) : Int = {
    // line is build back to front
    val lines1 = path2Line(path1.split(",")).reverse
    val lines2 = path2Line(path2.split(",")).reverse
    // first intersection is (0,0) central port
    val intersections = calcIntersections(lines1, lines2)

    val intersections2 = if (intersections.head._1 == Position(0, 0)) intersections.drop(1) else intersections

    val distances = intersections2.map{ case (pos, line1, line2) =>
      calcDistance(lines1, pos, line1) + calcDistance(lines2, pos, line2) }

    distances.min
  }
}
