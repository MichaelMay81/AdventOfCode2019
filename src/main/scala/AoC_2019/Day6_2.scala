package AoC_2019

object Day6_2 {

  sealed trait Tree
  case class Root() extends Tree
  case class Node(id: String, parentId: String) extends Tree

  case object Root { val id = "COM" }

  def string2OrbitTree(strOrbits: String): List[Node] = {
    strOrbits
      .split("\\s+|,")
      .map(s => s.split("\\)"))
      .map(l => Node(l(1), l(0)))
      .toList
  }

  @annotation.tailrec
  def sortByOrbit(orbits: List[Node],
                  processList: List[Tree] = List(Root()),
                  sortedOrbits: List[Node] = List(),
                 ): List[Node] = {
    processList match {
      case Nil => sortedOrbits
      case node :: rest =>
        val nodeId =
          node match {
            case Root() => Root.id
            case Node(nodeId, _) => nodeId
          }

        val childrenIds = orbits.filter(node => node.parentId == nodeId)

        sortByOrbit(
          orbits,
          rest ::: childrenIds,
          sortedOrbits ::: childrenIds)
    }
  }

  def calcNumberOfOrbits(orbits: List[Node]): Map[String, Int] = {
    @annotation.tailrec
    def cNoO(processList: List[Node],
             numOfOrbits: Map[String, Int] = Map((Root.id -> 0))
            ): Map[String, Int] =
      {
        processList match {
          case Nil => numOfOrbits
          case Node(id, parentId) :: rest =>
            val count = numOfOrbits(parentId) + 1
            cNoO(rest, numOfOrbits + (id -> count))
        }
      }

    val sorted = sortByOrbit(orbits)
    cNoO(sorted)
  }

  def calcTotalNumberOfOrbits(strOrbits: String) : Int = {
    val tree = string2OrbitTree(strOrbits)
    val noo = calcNumberOfOrbits(tree)
    noo.values.sum
  }
}
