package mtree

import org.scalatest._
import MTree.fromString

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

    "afg^^c^bd^e^^^".internalPathLength should be (9)
  }

  "postorder" should "return list with single value for single node" in {
    MTree('a').postorder should be(List('a'))
  }

  it should "return list of nodes with value after all children" in {
    "afg^^c^bd^e^^^".postorder should be(List('g', 'f', 'c', 'd', 'e', 'b', 'a'))
  }

  "lispyTree" should "return one element string if only one node without children" in {
    MTree("x").lispyTree should be("x")
  }

  it should "return root element as first element of list and children as consequential" in {
    MTree("x", List(MTree("y"), MTree("z"))).lispyTree should be ("(x y z)")
    MTree("a", List(MTree("b", List(MTree("c"))))).lispyTree should be("(a (b c))")
  }

  "fromLispy" should "return one element MTree if one element string passed" in {
    MTree.fromLispy("abc") should be(MTree("abc"))
  }

  it should "convert sublists into nodes with children" in {
    MTree.fromLispy("(aa bc)") should be (MTree("aa", List(MTree("bc"))))
  }

}
