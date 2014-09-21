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

  "addNode" should "create new tree if applied to End node" in {
    End.addValue(2) should be (Node(2))
  }

  it should "do nothing if value already inside" in {
    Node(2).addValue(2) should be(Node(2))
  }

  it should "add to left subtree if value is lesser than current" in {
    Node(2).addValue(1) should be(Node(2, Node(1), End))
  }

  it should "add to right subtree if value is greater than current" in {
    Node(2).addValue(3) should be(Node(2, End, Node(3)))
  }

  "fromList" should "create new binary search tree from list of elements" in {
    Tree.fromList(List(3, 2, 5, 7, 1)) should be (
      Node(3,
        Node(2,
          Node(1),
          End
        ),
        Node(5,
          End,
          Node(7)
        )
      )
    )

    Tree.fromList(List(5, 3, 18, 1, 4, 12, 21)).isSymmetric should be (true)

    Tree.fromList(List(3, 2, 5, 7, 4)).isSymmetric should be (false)
  }

  "symmetricBalancedTrees" should "generate all symmetric balanced trees" in {
    Tree.symmetricBalancedTrees(5, "x") should be (
      List(
        Node("x",
          Node("x",
            End,
            Node("x")
          ),
          Node("x",
            Node("x"),
            End
          )
        ),
        Node("x",
          Node("x",
            Node("x"),
            End
          ),
          Node("x",
            End,
            Node("x")
          )
        )
      )
    )
  }

}
