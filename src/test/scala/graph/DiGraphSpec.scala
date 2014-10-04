package graph

import org.scalatest._

class DiGraphSpec extends FlatSpec with Matchers {

  "fromString" should "create graph by given string" in {
    Digraph.fromString("[p>q, m>q, k, p>m]").toAdjacentForm should contain theSameElementsAs
      List(("m", List(("q", ()))), ("p", List(("m", ()), ("q", ()))), ("k", List()), ("q", List()))
  }

  "fromStringLabel" should "create graph with weighted labels" in {
    Digraph.fromStringLabel("[p>q/9, m>q/7, k, p>m/5]").toAdjacentForm should contain theSameElementsAs
      List(("m",List(("q",7))), ("p",List(("m",5), ("q",9))), ("k",List()), ("q",List()))

  }

}
