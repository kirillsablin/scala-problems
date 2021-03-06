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

  "hbalTrees" should "return single element list if height is zero" in {
    Tree.hbalTrees(0, "x") should be (List(End))
  }

  it should "return single element list if height is one" in {
    Tree.hbalTrees(1, "x") should be (List(Node("x")))
  }

  it should "return tree is created from subtrees of height minus one and minus two" in {
    Tree.hbalTrees(2, "x") should be (List(Node("x", Node("x"), End), Node("x", End, Node("x")), Node("x", Node("x"), Node("x"))))
  }


  "minHbalNodes" should "return 0 for zero height" in {
    Tree.minHbalNodes(0) should be (0)
  }

  it should "return 1 for one height" in {
    Tree.minHbalNodes(1) should be (1)
  }

  it should "return sum of previous two plus 1 for any other height" in {
    Tree.minHbalNodes(2) should be (2)
    Tree.minHbalNodes(3) should be (4)
    Tree.minHbalNodes(4) should be (7)
    Tree.minHbalNodes(5) should be (12)
    Tree.minHbalNodes(15) should be(1596)
  }

  "maxHbalHeight" should "return 0 for zero elements" in {
    Tree.maxHbalHeight(0) should be(0)
  }

  it should "return height before minHbalNodes would be greater than number of elements" in {
    Tree.maxHbalHeight(1) should be (1)
    Tree.maxHbalHeight(6) should be(3)
    Tree.maxHbalHeight(7) should be(4)
    Tree.maxHbalHeight(8) should be(4)
  }

  "hbalTreesWithNodes" should "return all height balanced trees with such amount of elements" in {
    Tree.hbalTreesWithNodes(4, "x").length should be(4)
    Tree.hbalTreesWithNodes(15, "x").length should be(1553)
  }

  "nodeCount" should "return zero for empty tree" in {
    End.nodeCount should be(0)
  }

  it should "return sum of leafCount for subnodes plus one for node" in {
    Node("x", Node("x"), Node("x", Node("x"), End)).nodeCount should be(4)
  }

  "leafCount" should "count only non-empty nodes without subtrees" in {
    Node('x', Node('x'), End).leafCount should be(1)
  }

  "leafList" should "collect leafs" in {
    Node('a', Node('b'), Node('c', Node('d'), Node('e'))).leafList should be (List('b', 'd', 'e'))
  }

  "internalList" should "collect nodes with one or two non emtpy children" in {
    End.internalList should be (List())
    Node('a', Node('b'), Node('c', Node('d'), Node('e'))).internalList should be(List('a', 'c'))
  }

  "atLevel" should "return empty list if there is no level" in {
    End.atLevel(0) should be(List())
    End.atLevel(1) should be(List())
  }

  it should "return empty list if level is bigger than height of tree" in {
    Node("x").atLevel(2) should be(List())
    Node("x", Node("y"), End).atLevel(3) should be(List())
  }

  it should "return value of root node at level one" in {
    Node("x", Node("y"), End).atLevel(1) should be(List("x"))
  }

  it should "return list of values on given level" in {
    Node('a', Node('b'), Node('c', Node('d'), Node('e'))).atLevel(2) should be(List('b', 'c'))
  }

  "completeBinaryTree" should "create empty tree for zero n" in {
     Tree.completeBinaryTree(0, "x") should be (End)
  }

  it should "create one element tree for one" in {
    Tree.completeBinaryTree(1, "x") should be(Node("x"))
  }

  it should "create complete binary tree" in {
    Tree.completeBinaryTree(2, "x") should be(Node("x", Node("x"), End))
    Tree.completeBinaryTree(3, "x") should be(Node("x", Node("x"), Node("x")))
    Tree.completeBinaryTree(6, "x") should be(Node("x", Node("x", Node("x"), Node("x")), Node("x", Node("x"), End)))
  }

  "layoutBinaryTree" should "convert Node to PositionedNode" in {
    Node('a', Node('b', End, Node('c')), Node('d')).layoutBinaryTree.toString should be(
       "T[3,1](a T[1,2](b . T[2,3](c . .)) T[4,2](d . .))"
    )
  }

  "layoutBinaryTree2" should "convert Node tp PositionedNode with constant width on each level" in {
    Node('a', Node('b', End, Node('c')), Node('d')).layoutBinaryTree2.toString should be(
      "T[3,1](a T[1,2](b . T[2,3](c . .)) T[5,2](d . .))"
    )
  }

  "height" should "return height of tree" in {
    End.height should be(0)
    Node("X").height should be(1)
    Node("x", End, Node("x", Node("x"), End)).height should be(3)
  }

  "toStringEx" should "generated empty string for End node" in {
    End.toStringEx should be("")
  }

  it should "return value for node without children" in {
    Node("x").toStringEx should be("x")
  }

  it should "return functional style for complex tree" in {
    Node('a', Node('b', Node('d'), Node('e')), Node('c', End, Node('f', Node('g'), End))).toStringEx should be(
     "a(b(d,e),c(,f(g,)))"
    )
  }

  "fromString" should "return End node on empty string" in {
    Tree.fromString("") should be(End)
  }

  it should "return single node if only one symbol passed" in {
    Tree.fromString("a") should be(Node('a'))
  }

  it should "throw exception on invalid input" in {
    a[IllegalArgumentException] should be thrownBy {
      Tree.fromString("aa")
    }

    a[IllegalArgumentException] should be thrownBy {
      Tree.fromString("a(")
    }

    a[IllegalArgumentException] should be thrownBy {
      Tree.fromString("a)")
    }

    a[IllegalArgumentException] should be thrownBy {
      Tree.fromString(",")
    }
  }

  it should "parse back strings produced be toStringEx" in {
    val tree = Node('a', Node('b'), Node('c', End, Node('d')))
    Tree.fromString(tree.toStringEx) should be(tree)

    val anotherTree = Node('a', Node('b', Node('d'), Node('e')), Node('c', End, Node('f', Node('g'), End)))
    Tree.fromString(anotherTree.toStringEx) should be (anotherTree)
  }

  "preorder" should "convert tree to sequence with node element comes before subnodes" in {
    End.preorder should be(List())

    Node("a", Node("b"), Node("c", End, Node("d"))).preorder should be (List("a", "b", "c", "d"))

    Tree.fromString("a(b(d,e),c(,f(g,)))").preorder should be(List('a', 'b', 'd', 'e', 'c', 'f', 'g'))
  }

  "inorder" should "convert tree to sequence with node element between left and right subnodes" in {
    End.inorder should be(List())

    Node("a", Node("b"), Node("c", End, Node("d"))).inorder should be (List("b", "a", "c", "d"))

    Tree.fromString("a(b(d,e),c(,f(g,)))").inorder should be (List('d', 'b', 'e', 'a', 'c', 'g', 'f'))
  }

  "preInTree" should "return End if both pre and in are empty lists" in {
    Tree.preInTree(List(), List()) should be (End)
  }

  it should "return constructed tree if lists are same size and consist of same elements" in {
    Tree.preInTree(List('a', 'b', 'd', 'e', 'c', 'f', 'g'), List('d', 'b', 'e', 'a', 'c', 'g', 'f')).toStringEx should be(
      "a(b(d,e),c(,f(g,)))"
    )
  }

  "toDotString" should "convert char tree to preorder representation with dots as empty nodes" in {
    End.toDotstring should be (".")
    Tree.fromString("a(b(d,e),c(,f(g,)))").toDotstring should be ("abd..e..c.fg...")
  }

  "fromDotstring" should "restore End node from one dot" in {
    Tree.fromDotstring(".") should be (End)
  }

  it should "restore tree from dotstring representation" in {
    Tree.fromDotstring("abd..e..c.fg...").toStringEx should be ("a(b(d,e),c(,f(g,)))")
  }
}
