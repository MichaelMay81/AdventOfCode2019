package AoC_2019

import AoC_2019.Day5.Intprog

object Day11 {
  sealed trait Color
  case class Black() extends Color
  case class White() extends Color

  sealed trait TurnDirection
  case class LeftTurn() extends TurnDirection
  case class RightTurn() extends TurnDirection

  sealed trait FacingDirection
  case class Up() extends FacingDirection
  case class Down() extends FacingDirection
  case class Left() extends FacingDirection
  case class Right() extends FacingDirection

  case class RoboPosition(x: Int, y: Int, dir: FacingDirection)

  case class Position(x: Int, y: Int)
  type Hull = Map[Position, Color]

  def IntToColor(i: Int): Color =
    i match {
      case 0 => Black()
      case 1 => White()
    }
  private def ColorToInt(c: Color): Int =
    c match {
      case _ : Black => 0
      case _ : White => 1
    }
  def IntToTurnDirection(i: Int): TurnDirection =
    i match {
      case 0 => LeftTurn()
      case 1 => RightTurn()
    }
  private def Turn(facing: FacingDirection, turning: TurnDirection): FacingDirection =
    (facing, turning) match {
      case (Up(), LeftTurn()) => Left()
      case (Left(), LeftTurn()) => Down()
      case (Down(), LeftTurn()) => Right()
      case (Right(), LeftTurn()) => Up()

      case (Up(), RightTurn()) => Right()
      case (Right(), RightTurn()) => Down()
      case (Down(), RightTurn()) => Left()
      case (Left(), RightTurn()) => Up()
    }
  private def Move(pos: RoboPosition): RoboPosition =
    pos.dir match {
      case _ : Up => pos.copy(y = pos.y - 1)
      case _ : Down => pos.copy(y = pos.y + 1)
      case _ : Left => pos.copy(x = pos.x - 1)
      case _ : Right => pos.copy(x = pos.x + 1)
    }

  def PrintOutHull(hull: Hull, roboOpt: Option[RoboPosition] = None): Unit = {
    val xHullMin = hull.keys.map(_.x).min
    val xHullMax = hull.keys.map(_.x).max
    val yHullMin = hull.keys.map(_.y).min
    val yHullMax = hull.keys.map(_.y).max

    val (xMin, xMax, yMin, yMax) =
      roboOpt match {
        case None =>
          (xHullMin, xHullMax, yHullMin, yHullMax)
        case Some(robo) =>
          (List(xHullMin, robo.x).min,
            List(xHullMax, robo.x).max,
            List(yHullMin, robo.y).min,
            List(yHullMax, robo.y).max)
      }

    val blackHull2 = (xMin to xMax).flatMap(x => (yMin to yMax).map(y => Position(x, y) -> Day11.Black())).toMap
    val completeHull = blackHull2 ++ hull

    val foobar = completeHull
      .groupBy(_._1.y)
      .map{ case (y, m) => (y, m.toList.sortBy(_._1.x))}
      .toList
      .sortBy(_._1)

    for (line <- foobar) {
      for (value <- line._2)
        roboOpt match {
          case Some(RoboPosition(value._1.x, value._1.y, Up())) =>  print("^")
          case Some(RoboPosition(value._1.x, value._1.y, Down())) =>  print("v")
          case Some(RoboPosition(value._1.x, value._1.y, Left())) =>  print("<")
          case Some(RoboPosition(value._1.x, value._1.y, Right())) =>  print(">")
          case _ =>
            value._2 match {
              case Black() => print(".")
              case White() => print("#")
            }
        }

      println()
    }
  }

  @scala.annotation.tailrec
  def EHPR_Test(prog: List[(Color, TurnDirection)],
               pos: RoboPosition = RoboPosition(0, 0, Up()),
               hull: Hull = Map()): Hull = {
    val newHull = hull + (Position(pos.x, pos.y) -> prog.head._1)
    val newPos = Move(pos.copy(dir = Turn(pos.dir, prog.head._2)))

    if (prog.length == 1)
      newHull
    else
      EHPR_Test(prog.tail, newPos, newHull)
  }

  @scala.annotation.tailrec
  def EmergencyHullPaintingRobot(prog: Intprog,
                                 pos: RoboPosition = RoboPosition(0, 0, Up()),
                                 hull: Hull = Map(),
                                 stepCount: Int = 1): (Hull, Int) = {
    import Day5.intinput

    // process
    val col = hull.getOrElse(Position(pos.x, pos.y), Black())
    val newProg = (ColorToInt(col) |> prog).process()

    // results
    val dir2go = IntToTurnDirection(newProg.output.head.toInt)
    val color2paint = IntToColor(newProg.output(1).toInt)

    // react
    val newHull = hull + (Position(pos.x, pos.y) -> color2paint)
    val newPos = Move(pos.copy(dir = Turn(pos.dir, dir2go)))

    // repeat...
    if (newProg.finished)
      (newHull, stepCount)
    else
      EmergencyHullPaintingRobot(newProg.copy(output = List()), newPos, newHull, stepCount + 1)
  }
}
