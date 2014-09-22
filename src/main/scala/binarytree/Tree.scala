package binarytree

sealed abstract class Tree[+T] {
  def isSymmetric:Boolean
  def isMirrorOf[V](other:Tree[V]):Boolean
  def addValue[U >: T <% Ordered[U]](x: U): Tree[U]
  def nodeCount:Int
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

  override def nodeCount: Int = left.nodeCount + right.nodeCount + 1
}
case object End extends Tree[Nothing] {

  override def isMirrorOf[V](other: Tree[V]): Boolean = other == End

  override def toString = "."

  override def isSymmetric:Boolean = true

  override def addValue[U >: Nothing <% Ordered[U]](x: U): Tree[U] = Node(x)

  override def nodeCount: Int = 0
}
object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}

object Tree {


  def symmetricBalancedTrees[T](n: Int, elem: T): List[Tree[T]] = cBalanced(n, elem).filter(_.isSymmetric)

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

  def hbalTrees[T](n: Int, elem: T): List[Tree[T]] =
    if (n == 0)
      List(End)
    else if (n == 1) {
      List(Node(elem))
    }
    else {
      val nMinusOne = hbalTrees(n-1, elem)
      val nMinusTwo = hbalTrees(n-2, elem)

      (nMinusOne flatMap (x => nMinusTwo flatMap (y => List(Node(elem, x, y), Node(elem, y, x))))) ++
        (nMinusOne flatMap (x => nMinusOne map (y => Node(elem, x, y))))
    }

  def minHbalNodesFrom(a:Int, b:Int):Stream[Int] = a #:: minHbalNodesFrom(b, a + b + 1)

  val minsHbalNodes = minHbalNodesFrom(0, 1)

  def minHbalNodes(n: Int):Int = (minsHbalNodes drop n).head

  def maxHbalHeight(n: Int):Int = (minsHbalNodes takeWhile( _ <= n)).length - 1

  def hbalTreesWithNodes[T](n: Int, elem: T):List[Tree[T]] = {
    val minHeight = Math.ceil(math.log(n+1) / math.log(2) ).toInt
    (minHeight to maxHbalHeight(n)).flatMap(h => hbalTrees(h, elem)).filter (_.nodeCount == n) .toList
  }

}
