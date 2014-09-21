package binarytree

sealed abstract class Tree[+T] {
  def isSymmetric:Boolean
  def isMirrorOf[V](other:Tree[V]):Boolean
  def addValue[U >: T <% Ordered[U]](x: U): Tree[U]
}
case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def isMirrorOf[V](other: Tree[V]): Boolean = other match {
    case End => false
    case Node(_, l, r) => left.isMirrorOf(r) && right.isMirrorOf(l)
  }

  override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"

  override def isSymmetric: Boolean = left isMirrorOf right

  override def addValue[U >: T <% Ordered[U]](x: U): Tree[U] =
    if (x < value)
      Node(value, left.addValue(x), right)
    else if (x > value)
      Node(value, left, right.addValue(x))
    else
      this
}
case object End extends Tree[Nothing] {

  override def isMirrorOf[V](other: Tree[V]): Boolean = other == End

  override def toString = "."

  override def isSymmetric:Boolean = true

  override def addValue[U >: Nothing <% Ordered[U]](x: U): Tree[U] = Node(x)
}
object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}

object Tree {
  def fromList[T <% Ordered[T]](elements: List[T]):Tree[T] =
    elements.foldLeft[Tree[T]](End)((acc:Tree[T], current:T) => acc.addValue(current))

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
