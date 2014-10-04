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

}
