package binarytree

sealed abstract class Tree[+T] {

  def isSymmetric:Boolean

  def isMirrorOf[V](other:Tree[V]):Boolean

  def addValue[U >: T <% Ordered[U]](x: U): Tree[U]

  def nodeCount:Int

  def leafCount:Int = leafList.length

  def leafList:List[T]

  def internalList:List[T]

  def atLevel(level: Int):List[T]

  val height:Int

}
case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {

  def layoutBinaryTree: PositionedNode[T] = {
    def recursiveMakeLayout(node:Tree[T], level:Int, before:Int):PositionedNode[T] = node match {
      case Node(currentValue, currentLeft, currentRight) =>
        val positionedLeft = if (currentLeft != End) recursiveMakeLayout(currentLeft, level + 1, before) else End
        val leftCount = positionedLeft.nodeCount
        val positionedRight = if (currentRight != End) recursiveMakeLayout(currentRight, level + 1, before + leftCount + 1) else End

        new PositionedNode(currentValue, positionedLeft, positionedRight, before + 1 + leftCount, level)
    }

    recursiveMakeLayout(this, 1, 0)
  }

  def layoutBinaryTree2: PositionedNode[T] = {
    def pow2(i:Int):Int = (1 to i).foldLeft(1)((acc,_) => acc*2)
    def constructLayout(current:Tree[T], absoluteHeight:Int, offset:Int, level:Int):PositionedNode[T] = current match {
      case node:Node[T] =>
        val newLeft = if (node.left != End) constructLayout(node.left, absoluteHeight - 1, offset, level + 1) else End
        val newRight = if (node.right != End) constructLayout(node.right, absoluteHeight - 1, offset + pow2(absoluteHeight - 1), level + 1) else End
        new PositionedNode[T](node.value, newLeft, newRight, offset - 2 + pow2(absoluteHeight - 1), level)
    }

    constructLayout(this, height, 1, 1)
  }

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

  private[this] def isLeaf = left == End && right == End

  override def leafList: List[T] = if (isLeaf) List(value) else left.leafList ++ right.leafList

  override def internalList: List[T] = if (isLeaf) List() else value :: (right.internalList ::: left.internalList)

  override def atLevel(level: Int): List[T] = level match {
    case _ if level < 1 => List()
    case 1 => List(value)
    case _ => left.atLevel(level - 1) ::: right.atLevel(level - 1)
  }

  lazy val height: Int = math.max(left.height, right.height) + 1
}
case object End extends Tree[Nothing] {

  override def isMirrorOf[V](other: Tree[V]): Boolean = other == End

  override def toString = "."

  override def isSymmetric:Boolean = true

  override def addValue[U >: Nothing <% Ordered[U]](x: U): Tree[U] = Node(x)

  override def nodeCount: Int = 0

  override def leafList: List[Nothing] = List()

  override def internalList: List[Nothing] = List()

  override def atLevel(level: Int): List[Nothing] = List()

  override val height:Int = 0
}

class PositionedNode[+T](override val value: T, override val left: Tree[T], override val right: Tree[T], val x: Int, val y: Int) extends Node[T](value, left, right) {
  override def toString = "T[" + x.toString + "," + y.toString + "](" + value.toString + " " + left.toString + " " + right.toString + ")"
}

object Node {

  def apply[T](value: T): Node[T] = Node(value, End, End)

}



object Tree {
  def completeBinaryTree[T](n: Int, elem: T):Tree[T] = {
    def subtreeForNumber(number:Int):Tree[T] =
      if (number > n) End else Node(elem, subtreeForNumber(number*2), subtreeForNumber(number*2+1))

    subtreeForNumber(1)
  }


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
