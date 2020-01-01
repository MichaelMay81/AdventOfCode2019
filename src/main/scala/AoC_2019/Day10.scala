package AoC_2019

object Day10 {

  case class Asteroid(x: Int, y: Int) {
    def -(other: Asteroid): Asteroid = Asteroid(x - other.x, y - other.y)

    def dot(other: Asteroid): Int = x * other.x + y * other.y

    // determinant
    def det(other: Asteroid): Int = x * other.y - y * other.x

    def length(): Double = math.sqrt(math.pow(x, 2) + math.pow(y, 2))

    def angle(other: Asteroid, ccw: Boolean = false): Double = {
      val angle = math.atan2(this.det(other), this.dot(other))
      val angle2 = if (ccw) -angle else angle
      if (angle2 < 0)
        (2 * math.Pi) + angle2
      else angle2
    }
  }

  case class Line(ast1: Asteroid, ast2: Asteroid) {
    def distanceTo(ast: Asteroid): Double = (ast.det(ast2 - ast1) + ast2.det(ast1)).abs / (ast2 - ast1).length()

    def posOnLine(ast: Asteroid): Double = (ast - ast1).dot(ast2 - ast1).toDouble / (ast2 - ast1).dot(ast2 - ast1)
  }

  def computeAsteroidLocations(map: String): (List[Asteroid], Option[Asteroid]) = {
    val lines = map.split("\n").map(l => l.zipWithIndex).zipWithIndex

    val m_station = lines
      .flatMap { case (l, y) => l
        .find { case (c, _) => c == 'X' }
        .map { case (_, x) => Asteroid(x, y) }
      }.headOption

    val asteroids = lines
      .flatMap { case (l, y) => l
        .filter { case (c, _) => c == '#' }
        .map { case (_, x) => Asteroid(x, y) }
      }.toList

    (asteroids, m_station)
  }

  def computeBestLocation(map: String): (Asteroid, Int) = computeBestLocation(computeAsteroidLocations(map)._1)

  def computeBestLocation(asteroids: List[Asteroid]): (Asteroid, Int) = {
    val ast_size_half = 0

    // all lines between two asteroids
    val lines = asteroids
      .combinations(2)
      .map(l => Line(l.head, l(1)))
      .toList

    // distances of asteroids to the lines
    val dists = lines.flatMap(l => asteroids
      .filter(a => a != l.ast1 && a != l.ast2)
      .map(a => (l, a, l.distanceTo(a))))

    // asteroids on lines
    val onLine = dists
      .filter { case (_, _, d) => d <= ast_size_half }

    // position of the asteroid on line
    val posOnLine = onLine.map { case (l, a, _) => (l, a, l.posOnLine(a)) }

    // only asteroids between line-ends
    val inBetween = posOnLine.filter { case (l, a, p) => p > 0 && p < 1 }

    // get all lines
    val brokenLines = inBetween.map { case (l, _, _) => l }.distinct

    // count how many asteroids cannot be seen
    val counts = asteroids.map(a => (a, brokenLines.count(l => a == l.ast1 || a == l.ast2))).sortBy(_._2)

    (counts.head._1, asteroids.length - counts.head._2 - 1)
  }

  def computeVaporizationByGiantLaser(map: String): List[Asteroid] = {
    val (asteroids, stationOption) = computeAsteroidLocations(map)
    stationOption match {
      case None => List()
      case Some(station) => computeVaporizationByGiantLaser(asteroids, station)
    }
  }

  def computeVaporizationByGiantLaser(asteroids: List[Asteroid], station: Asteroid): List[Asteroid] = {
    val yAxis = Asteroid(0, -1)
    val anglesAndDist = asteroids.map(a => (a, yAxis.angle(a - station), (a - station).length()))
    val angleAndRingId = anglesAndDist
      .groupBy(_._2)
      .flatMap { case (_, v) => v
        .sortBy(_._3)
        .zipWithIndex
        .map { case ((ast, ang, _), id) => (ast, id, ang) }
      }

    val sorted = angleAndRingId.toList.sortBy { case (_, ang, id) => (ang, id) }
    sorted.map(_._1)
  }
}
