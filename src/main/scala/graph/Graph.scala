package graph

class Graph[T, U] extends GraphBase[T, U, Graph[T, U]] {
  def isIsomorphicTo(other: Graph[T, U]):Boolean =
    if (nodes.size != other.nodes.size || edges.size != other.edges.size)
      false
    else {
      val original = nodes.keys.toList
      val permMaps = other.nodes.keys.toList.permutations.map(e => Map(original zip e: _*)).toList

      !permMaps.forall(m => transformNodes(m) != other)
    }

  def transformNodes[NewT](map: Map[T, NewT]):Graph[NewT, U] = {
    Graph.termLabel(nodes.keys.map(map).toList, edges.map(e => (map(e.n1.value), map(e.n2.value), e.value)))
  }

  def edgesToRestOfGraph(nodes:Set[T]): List[Edge] =
      nodes.toList.flatMap( n => this.nodes(n).adj).filter( e => ! (nodes.contains(e.n1.value) == nodes.contains(e.n2.value)))

  def minimalSpanningTree(implicit ev:Ordering[U]):Graph[T, U] = {

    def calculateResult(nodes: Set[T], edges: List[Edge]):Graph[T, U] =
      if (nodes.size == this.nodes.size) {
        Graph.termLabel(nodes.toList, edges.map(_.toTuple))
      } else {
        val newEdge = edgesToRestOfGraph(nodes).min (new Ordering[Edge] {
          override def compare(x: Edge, y: Edge): Int = ev.compare(x.value, y.value)
        })
        calculateResult(nodes + newEdge.n1.value + newEdge.n2.value, newEdge :: edges)
      }

    calculateResult(Set(nodes.head._1), List())
  }

  def spanningTrees:List[Graph[T, U]] = {
    def spanningTreesR(nodes:Set[T], edges:List[Edge]):List[Graph[T, U]] =
    if (nodes.size == this.nodes.size)
      List(Graph.termLabel(nodes.toList, edges.map(_.toTuple)))
    else {
      edgesToRestOfGraph(nodes).flatMap(e => spanningTreesR(nodes + e.n1.value + e.n2.value, e::edges))
    }

    spanningTreesR(Set(nodes.head._1), List()).distinct.foreach(a => println(a.hashCode()))
    spanningTreesR(Set(nodes.head._1), List()).distinct
  }

  override def equals(o: Any) = o match {
    case g: Graph[_,_] => super.equals(g)
    case _ => false
  }

  def edgeTarget(e: Edge, n: Node): Option[Node] =
    if (e.n1 == n) Some(e.n2)
    else if (e.n2 == n) Some(e.n1)
    else None

  def addEdge(n1: T, n2: T, value: U) = {
    val e = new Edge(nodes(n1), nodes(n2), value)
    edges = e :: edges
    nodes(n1).adj = e :: nodes(n1).adj
    nodes(n2).adj = e :: nodes(n2).adj
  }

  override def edgeSeparator: String = Graph.edgeSeparator
}

object Graph extends GraphObjBase {

  type GraphClass[T, U] = Graph[T, U]

  def termLabel[T, U](nodes: List[T], edges: List[(T,T,U)]) = {
    val g = new Graph[T, U]
    nodes.map(g.addNode)
    edges.map(v => g.addEdge(v._1, v._2, v._3))
    g
  }
  def adjacentLabel[T, U](nodes: List[(T, List[(T,U)])]) = {
    val g = new Graph[T, U]
    for ((v, a) <- nodes) g.addNode(v)
    for ((n1, a) <- nodes; (n2, l) <- a) {
      if (!g.nodes(n1).neighbors.contains(g.nodes(n2)))
        g.addEdge(n1, n2, l)
    }
    g
  }

  override def edgeSeparator: String = "-"
}