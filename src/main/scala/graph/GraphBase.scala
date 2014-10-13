package graph

abstract class GraphBase[T, U, +Repr] {
  case class Edge(n1: Node, n2: Node, value: U) {
    def toTuple = (n1.value, n2.value, value)
  }
  case class Node(value: T) {

    def degree = adj.size

    var adj: List[Edge] = Nil
    // neighbors are all nodes adjacent to this node.
    def neighbors: List[Node] = adj.map(edgeTarget(_, this).get)
  }

  def nodesByDegree:List[Node] = nodes.values.toList.sortWith((n1, n2) => n1.degree > n2.degree)

  def colorNodes:List[(Node, Int)] = {
    def colorR(prevColor:Int, nodesToColor:List[Node], coloredNodes:List[(Node, Int)]):List[(Node,Int)] = {
      if (nodesToColor.isEmpty)
        coloredNodes
      else {
        val (toColor, rest) = nodesToColor.foldLeft[(List[Node], List[Node])]((List(), List()))( (acc, node) =>
          if (acc._1.forall( n => n.neighbors.find( neighbor => neighbor.value == node.value).isEmpty))
            (node::acc._1, acc._2)
          else
            (acc._1, node::acc._2)
        )

        colorR(prevColor+1, rest, coloredNodes ++ toColor.map( (_, prevColor + 1)))
      }
    }

    colorR(0, nodesByDegree, List())
  }


  var nodes: Map[T, Node] = Map()
  var edges: List[Edge] = Nil

  // If the edge E connects N to another node, returns the other node,
  // otherwise returns None.
  def edgeTarget(e: Edge, n: Node): Option[Node]

  override def equals(o: Any) = o match {
    case g: GraphBase[_,_,_] => nodes.keys.toList.diff(g.nodes.keys.toList) == Nil &&
      edges.map(_.toTuple).diff(g.edges.map(_.toTuple)) == Nil
    case _ => false
  }

  override def hashCode(): Int = nodes.keys.foldLeft(0)( (acc, elem) => acc + elem.hashCode()) +
    edges.map(_.hashCode()).sum

  def addNode(value: T) = {
    val n = new Node(value)
    nodes = Map(value -> n) ++ nodes
    n
  }

  def toTermForm:(List[T], List[(T, T, U)]) = (nodes.keys.toList.reverse,
    edges.map ( e => (e.n1.value, e.n2.value, e.value)).reverse)

  def toAdjacentForm:List[(T, List[(T, U)])] =
    nodes.toList.map ( n => (n._1, n._2.adj.map( e => (edgeTarget(e, n._2).get.value, e.value)) ))

  override def toString: String = nodes.values.map (n => if (n.adj.isEmpty) n.value.toString else {
    n.adj.filter ( _.n1 == n) map ( e => n.value.toString + edgeSeparator + e.n2.value.toString +
      (if (e.value.isInstanceOf[Unit]) "" else "/" + e.value.toString)) mkString ", "
  }) filter (_.length > 0) mkString("[", ", ", "]")

  def edgeSeparator:String

  def findPaths(start: T, end: T):List[List[T]] = {
    def findR(current:List[T], soFar:List[List[T]]):List[List[T]] = {
      val soFarWithCurrent = if (current.head == end) current.reverse ::soFar else soFar

      nodes(current.head).neighbors.filter( v => !current.contains(v.value) ).foldLeft(soFarWithCurrent)(
        (soFarWithOther, node) => findR(node.value::current, soFarWithOther)
      )
    }

    findR(List(start), List())
  }

  def findCycles(start: T):List[List[T]] =
    nodes(start).neighbors flatMap( (next) => findPaths(next.value, start)) filter( _.length > 2) map (start::_)




}