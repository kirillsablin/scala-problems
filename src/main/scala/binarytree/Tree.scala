package binarytree

sealed abstract class Tree[+T]
case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
}
case object End extends Tree[Nothing] {
  override def toString = "."
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
