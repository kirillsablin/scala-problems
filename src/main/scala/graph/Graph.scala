package graph

class Graph[T, U] extends GraphBase[T, U] {

  def minimalSpanningTree(implicit ev:Ordering[U]):Graph[T, U] = {
    def edgesToRestOfGraph(nodes:Set[T]): List[Edge] =
        nodes.toList.flatMap( n => this.nodes(n).adj).filter( e => ! (nodes.contains(e.n1.value) && nodes.contains(e.n2.value)))

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
    type SameGraph = Graph[T, U]
    def spanningTreesR(left:SameGraph, right:SameGraph):List[SameGraph] = ???

//    spanningTreesR(Graph.term(List(nodes.head.)))
    spanningTreesR(this, this)
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