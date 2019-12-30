package AoC_2019

object Day10 {
  case class Asteroid(x: Int, y: Int) {
    def -(other: Asteroid): Asteroid = Asteroid(x - other.x, y - other.y)
    def dot(other: Asteroid): Int = x * other.x + y * other.y
  }
  case class Line(ast1: Asteroid, ast2: Asteroid) {
    //val length: Double = math.sqrt(math.pow(ast2.x-ast1.x, 2) + math.pow(ast2.y-ast1.y, 2))

    def distanceTo(ast: Asteroid): Double = distanceTo(ast.x, ast.y, ast1.x, ast1.y, ast2.x, ast2.y)
    def posOnLine(ast: Asteroid): Double = (ast - ast1).dot(ast2 - ast1).toDouble / (ast2 - ast1).dot(ast2 - ast1)

    private def distanceTo(x: Int, y: Int, x1: Int, y1: Int, x2: Int, y2: Int): Double =
      ((y2 -y1)*x - (x2 - x1)*y + x2*y1 - y2*x1).abs / math.sqrt(math.pow(y2 - y1, 2) + math.pow(x2 - x1, 2))
  }

  def computeAsteroidLocations(map: String): List[Asteroid] = {
    val lines = map.split("\n").map(l => l.zipWithIndex).zipWithIndex
    lines
      .flatMap{ case (l, y) => l
        .filter{ case (c, _) => c == '#' }
        .map{ case (_, x) => Asteroid(x, y) }
        .toList}.toList
  }

  def computeBestLocation(map: String): (Asteroid, Int) = {
    val ast_size_half = 0
    val asteroids = computeAsteroidLocations(map)

    // all lines between two asteroids
    val lines = asteroids
      .combinations(2)
      .map(l => Line(l(0), l(1)))
      .toList

    // distances of asteroids to the lines
    val dists = lines.flatMap(l => asteroids
      .filter(a => a != l.ast1 && a != l.ast2)
      .map(a => (l, a, l.distanceTo(a))))

    // asteroids on lines
    val onLine = dists
      .filter{ case (_, _, d) => d <= ast_size_half}

    // position of the asteroid on line
    val posOnLine = onLine.map{ case (l, a, _) => (l, a, l.posOnLine(a)) }

    // only asteroids between line-ends
    val inBetween = posOnLine.filter{ case (l, a, p) => p > 0 && p < 1 }

    // get all lines
    val brokenLines = inBetween.map{ case (l, _, _) => l}.distinct

    // count how many asteroids cannot be seen
    val counts = asteroids.map(a => (a, brokenLines.count(l => a == l.ast1 || a == l.ast2))).sortBy(_._2)

    (counts.head._1, asteroids.length - counts.head._2 - 1)
  }
}
