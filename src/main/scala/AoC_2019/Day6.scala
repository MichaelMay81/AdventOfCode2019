package AoC_2019

object Day6 {

  case class Node(parentId: String)

  def string2OrbitTree(strOrbits: String) : Map[String, Node] = {
    strOrbits
      .split("\\s+|,")
      .map(s => s.split("\\)"))
      .map(l => (l(1), Node(l(0))))
      .toMap
  }

  @annotation.tailrec
  def calcNumberOfOrbits(orbits: Map[String, Node],
                         processList: List[String] = List("COM"),
                         numOfOrbits: Map[String, Int] = Map()
                        ): Map[String, Int] =
  {
    processList match {
      case Nil => numOfOrbits
      case nodeId :: rest =>
        val childrenIds = orbits.filter(tup => tup._2.parentId == nodeId).keys.toList

        val newCount =
          if (nodeId == "COM") ("COM" -> 0)
          else
          {
            val parentId = orbits(nodeId).parentId
            val count = numOfOrbits(parentId) + 1
            (nodeId -> count)
          }

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
