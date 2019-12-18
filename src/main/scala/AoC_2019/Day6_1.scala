package AoC_2019

object Day6_1 {

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
  def calcNumberOfOrbits(orbits: List[Node],
                         processList: List[Tree] = List(Root()),
                         numOfOrbits: Map[String, Int] = Map()
                        ): Map[String, Int] =
  {
    processList match {
      case Nil => numOfOrbits
      case node :: rest =>
        val newCount =
          node match {
            case Root() =>
              (Root.id -> 0)
            case Node(nodeId, parentId) =>
              val count = numOfOrbits(parentId) + 1
              (nodeId -> count)
          }

        val childrenIds = orbits.filter(node => node.parentId == newCount._1)

        calcNumberOfOrbits(
          orbits,
          rest ::: childrenIds,
          numOfOrbits + newCount)
    }
  }

  def calcTotalNumberOfOrbits(strOrbits: String) : Int = {
    val tree = string2OrbitTree(strOrbits)
    val noo = calcNumberOfOrbits(tree)
    noo.values.sum
  }
}
