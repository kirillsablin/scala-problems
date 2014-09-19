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

}
