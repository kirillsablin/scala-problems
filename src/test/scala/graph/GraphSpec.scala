package graph

import org.scalatest._

class GraphSpec extends FlatSpec with Matchers {

  "toTermForm" should "return list with nodes and list with edges" in {
    val nodes = List('a', 'b', 'c')
    val edges = List(('a', 'b', ()), ('a', 'c', ()))
    Graph.termLabel(nodes, edges).toTermForm should be((nodes, edges))
  }

  "fromString" should "create graph by given string" in {
    val (nodes, edges) = Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]").toTermForm
    nodes should contain theSameElementsAs List("d", "k", "h", "c", "f", "g", "b")
    edges should contain theSameElementsAs List(("h", "g", ()), ("k", "f", ()), ("f", "b", ()), ("g", "h", ()),
      ("f", "c", ()), ("b", "c", ()))
  }

  it should "throw exceptions if begins and ends with not square braces" in {
    an[AssertionError] shouldBe thrownBy(
      Graph.fromString("b-c, h-g]")
    )

    an[AssertionError] shouldBe thrownBy(
      Graph.fromString("[b-c, h-g")
    )
  }

  "toString" should "convert unlabeled graph to string" in {
    val graphWithoutLabel = Graph.term(List("a", "b", "c", "d"), List(("a", "b"), ("a", "c")))

    graphWithoutLabel.toString should be ("[d, a-c, a-b]")
  }

  it should "convert labeled graph to string" in {
    val graphWithLabel = Graph.termLabel(List("a", "b", "c", "d"), List(("a", "b", 9), ("a", "c", 10)))

    graphWithLabel.toString should be ("[d, a-c/10, a-b/9]")

  }

  "findCycles" should "find all cycles" in {
    Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]").findCycles("f") should contain theSameElementsAs
      List(List("f", "c", "b", "f"), List("f", "b", "c", "f"))
  }

  "spanningTrees" should "return list of all spanning trees" in {
    Graph.fromString("[a-b, b-c, a-c]").spanningTrees should contain theSameElementsAs
      List(Graph.fromString("[a-b, b-c]"),Graph.fromString("[a-c, b-c]"),Graph.fromString("[a-b, a-c]") )
  }

  "minimalSpanningTree" should "return graph which contains all nodes and subset of edges with minimal sum length" in {
    Graph.fromStringLabel("[a-b/1, b-c/2, a-c/3]").minimalSpanningTree should be(
      Graph.fromStringLabel("[a-b/1, b-c/2]")
    )
  }

  "transformNodes" should "rename nodes" in {
    Graph.fromString("[a-b, b-c, a-c]").transformNodes(Map("a" -> 1, "b" -> 2, "c" ->3)).toString should be (
      "[1-2, 1-3, 2-3]"
    )
  }

  "isIsomorphicTo" should "return false if count of nodes or edges not equals" in {
    Graph.fromString("[a-b, b-c]") isIsomorphicTo Graph.fromString("[b-c]") should be (false)
    Graph.fromString("[a-b, b-c]") isIsomorphicTo Graph.fromString("[b-c, a]") should be (false)
  }

  it should "return true if exist nodes transformation to equal graphs" in {
    Graph.fromString("[a-b]").isIsomorphicTo(Graph.fromString("[5-7]")) should be (true)

    Graph.fromString("[a-b, b-c]") isIsomorphicTo Graph.fromString("[b-c, c-a]") should be (true)

    Graph.fromString("[a-b, c-d]") isIsomorphicTo Graph.fromString("[b-c, c-d, a]") should be (false)
  }

  "degree" should "return count of connected edjes" in {
    Graph.fromString("[a-b, b-c, a-c, a-d]").nodes("a").degree should be (3)
  }

  "nodesByDegree" should "return nodes in order of decreasing degree" in {
    Graph.fromString("[a-b, b-c, a-c, a-d]").nodesByDegree.toString should be(
      "List(Node(a), Node(c), Node(b), Node(d))"
    )
  }

  "colorNodes" should "return nodes with selected color" in {
    Graph.fromString("[a-b, b-c, a-c, a-d]").colorNodes.toString should be (
      "List((Node(a),1), (Node(b),2), (Node(d),2), (Node(c),3))"
    )
  }

  "nodesByDepthFrom" should "return list of nodes in depth-first order" in {
    Graph.fromString("[a-b, b-c, e, a-c, a-d]").nodesByDepthFrom("d") should be (
      List("c", "b", "a", "d")
    )
  }

  "splitGraph" should "return list of connected components" in {
    Graph.fromString("[a-b, c]").splitGraph should be (
      List(List("a", "b"), List("c"))
    )
  }

  "isBiporate" should "determine whether a given graph is biporate" in {
    Graph.fromString("[a-b, b-c, c-a]").isBipartite should be(false)
    Graph.fromString("[a-b, b-c, d]").isBipartite should be (true)
    Graph.fromString("[a-b, b-c, d, e-f, f-g, g-e, h]").isBipartite should be (false)
  }

}
