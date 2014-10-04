package graph

abstract class GraphObjBase {
  type GraphClass[T, U]
  def addLabel[T](edges: List[(T, T)]) =
    edges.map(v => (v._1, v._2, ()))
  def term[T](nodes: List[T], edges: List[(T,T)]) =
    termLabel(nodes, addLabel(edges))
  def termLabel[T, U](nodes: List[T], edges: List[(T,T,U)]): GraphClass[T, U]
  def addAdjacentLabel[T](nodes: List[(T, List[T])]) =
    nodes.map(a => (a._1, a._2.map((_, ()))))
  def adjacent[T](nodes: List[(T, List[T])]) =
    adjacentLabel(addAdjacentLabel(nodes))
  def adjacentLabel[T, U](nodes: List[(T, List[(T,U)])]): GraphClass[T, U]

  def baseFromString[U](nodesExtractor:(String => List[String]),
                        edgesExtractor:(String => (String, String, U)))(s:String):GraphClass[String, U] = {
    assert(s.charAt(0) == '[')
    assert(s.charAt(s.length - 1) == ']')
    val withoutBorders = s.substring(1, s.length-1)

    val parts = withoutBorders.split("\\s*,\\s").toList

    val nodes = parts.flatMap(nodesExtractor).distinct

    val edges = parts.filter( _.indexOf(edgeSeparator) != -1).map( edgesExtractor )

    termLabel(nodes, edges)
  }

  def fromString(s:String):GraphClass[String, Unit] = {
    baseFromString(_.split(edgeSeparator).toList, _.split(edgeSeparator).toList match {
      case a::b::Nil => (a, b, ())
    })(s)
  }

  def fromStringLabel(s: String):GraphClass[String,Int] = {
    baseFromString( _.split("/")(0).split(edgeSeparator).toList, _.split("/").toList match {
      case nodes::cost::Nil => nodes.split(edgeSeparator).toList match {
        case a::b::Nil => (a, b, cost.toInt)
      }
    })(s)
  }

  def edgeSeparator:String
}