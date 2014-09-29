package mtree

import org.scalatest._

class MTreeSpec extends FlatSpec with Matchers {
  "nodeCount" should "return count of all nodes" in {
    MTree('a').nodeCount should be(1)

    MTree('a', List(MTree('f'))).nodeCount should be (2)
  }

  "toString" should "convert tree to string" in {
    MTree('a').toString should be ("a^")
    MTree('a', List(MTree('f', List(MTree('g'))), MTree('c'), MTree('b', List(MTree('d'), MTree('e'))))).toString should be(
      "afg^^c^bd^e^^^"
    )

  }

  "mtree" should "be able to be created from string implicitly" in {
    val mtree:MTree[Char] = "afg^^c^bd^e^^^"

    mtree.toString should be ("afg^^c^bd^e^^^")
  }

  "internalPath" should "return sum of lengths of all paths from root to each node" in {
    import MTree.fromString

    "afg^^c^bd^e^^^".internalPathLength should be (9)
  }

}
