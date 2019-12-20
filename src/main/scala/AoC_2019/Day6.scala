package AoC_2019

object Day6 {

  sealed trait Tree
  case class Root() extends Tree
  case class Node(id: String, parentId: String) extends Tree

  case object Root { val id = "COM" }

  def string2OrbitTree(strOrbits: String): Map[String, Node] = {
    strOrbits
      .split("\\s+|,")
      .map(s => s.split("\\)"))
      .map(l => (l(1), Node(l(1), l(0))))
      .toMap
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
    val noo = calcNumberOfOrbits(tree.values.toList)
    noo.values.sum
  }

  def calcDistance(strOrbits: String,
                   nodeId1: String,
                   nodeId2: String): Int =
  {
    val tree = string2OrbitTree(strOrbits)

    @annotation.tailrec
    def getAllAncesters(n: Node, result: List[String] = List()): List[String] = {
      n.parentId match {
        case Root.id => Root.id :: result
        case parentId =>
          val parent = tree(parentId)
          getAllAncesters(parent, parentId :: result)
      }
    }

    val anc1 = getAllAncesters(tree(nodeId1)).reverse
    val anc2 = getAllAncesters(tree(nodeId2)).reverse

    val perm = anc1.flatMap(id1 => anc2.map(id2 => (id1, id2)))
    val first_similar = perm.filter{case (id1, id2) => id1 == id2}.head._1

    val steps1 = anc1.indexOf(first_similar)
    val steps2 = anc2.indexOf(first_similar)

    steps1 + steps2
  }
}
