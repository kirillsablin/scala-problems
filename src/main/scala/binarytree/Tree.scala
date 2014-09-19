package binarytree

sealed abstract class Tree[+T] {
  def isSymmetric:Boolean
  def isMirrorOf[V](other:Tree[V]):Boolean
}
case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def isMirrorOf[V](other: Tree[V]): Boolean = other match {
    case End => false
    case Node(_, l, r) => left.isMirrorOf(r) && right.isMirrorOf(l)
  }

  override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"

  override def isSymmetric: Boolean = left isMirrorOf right
}
case object End extends Tree[Nothing] {

  override def isMirrorOf[V](other: Tree[V]): Boolean = other == End

  override def toString = "."

  override def isSymmetric:Boolean = true
}
object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}

object Tree {
  def cBalanced[T](n: Int, elem: T):List[Tree[T]] = {
    def combineSubTrees(left: List[Tree[T]], right: List[Tree[T]]): List[Node[T]] =
      for (l <- left; r <- right) yield Node(elem, l, r)

    if (n == 0)
      List(End)
    else if ((n - 1) % 2 == 0) {
      val subTrees = cBalanced((n - 1) / 2, elem)
      combineSubTrees(subTrees, subTrees)
    } else {
      val smallerSubTrees = cBalanced((n - 2) / 2, elem)
      val biggerSubTrees = cBalanced(n / 2, elem)
      combineSubTrees(smallerSubTrees, biggerSubTrees) ++ combineSubTrees(biggerSubTrees, smallerSubTrees)
    }
  }

}
