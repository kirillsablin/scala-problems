package binarytree

import org.scalatest._

class TreeSpec extends FlatSpec with Matchers {

  "cBalanced" should "generate list with empty tree if n is zero" in {
    Tree.cBalanced(0, "x") should be (List(End))
  }

  it should "generate list with one element tree if n is one" in {
    Tree.cBalanced(1, "x") should be (List(Node("x")))
  }

  it should "generate list with all possible solutions for n greater then one" in {
    Tree.cBalanced(2, "x") should be (List(Node("x", End, Node("x")), Node("x", Node("x"), End)))
    Tree.cBalanced(3, "x") should be (List(Node("x", Node("x"), Node("x"))))
    Tree.cBalanced(4, "x") should be (
      List(
        Node("x", Node("x"), Node("x", End, Node("x"))),
        Node("x", Node("x"), Node("x", Node("x"), End)),
        Node("x", Node("x", End, Node("x")), Node("x")),
        Node("x", Node("x", Node("x"), End), Node("x"))
      )
    )
  }

  "isMirrorOf" should "return true if both are empty trees" in {
    End isMirrorOf End should be (true)
    End isMirrorOf Node("x") should be (false)
  }

  it should "return false if left is node and right is empty tree" in {
    Node("x") isMirrorOf End should be (false)
  }

  it should "return true if left part one mirrors right part of another and vice versa" in {
    Node("x", End, Node("a")) isMirrorOf Node("a", Node("a"), End) should be (true)
    Node("x", End, Node("a")) isMirrorOf Node("a", Node("a"), Node("b")) should be (false)
  }

  "isSymmetric" should "be true on empty node" in {
    End.isSymmetric should be (true)
  }

  it should "be true on tree of one element" in {
    Node("x").isSymmetric should be (true)
  }

  it should "return true only if symmetric regardless of values" in {
    Node("x", End, Node("x")).isSymmetric should be(false)
    Node("x", Node("y"), Node("x")).isSymmetric should be(true)

  }

}
