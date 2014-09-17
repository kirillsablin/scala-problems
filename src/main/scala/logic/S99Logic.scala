package logic

import scala.annotation.tailrec
import scala.collection.mutable

class S99Logic(val source:Boolean) {
  def and(other:Boolean) = source && other
  def or(other: Boolean) = source || other
}

object S99Logic {
  def and(a: Boolean, b: Boolean):Boolean = a && b
  def or(a: Boolean, b: Boolean):Boolean = a || b
  def nand(a: Boolean, b: Boolean) = !(a && b)
  def notOp(a: Boolean): Boolean = !a

  implicit def toS99Logic(b: Boolean) = new S99Logic(b)

  private val booleans = List(true, false)

  def table2(f: (Boolean, Boolean) => Boolean):List[((Boolean, Boolean), Boolean)] =
    for (a <- booleans; b <- booleans) yield ((a, b), f(a, b))

  def gray(n: Int):List[String] =
    if (n == 1)
      List("0", "1")
    else {
      val prev = gray(n - 1)

      prev.map("0"+ _)  ++ prev.reverse.map("1" + _)
    }

  def huffman(symbols: List[(String, Int)]):List[(String, String)] = {

    sealed abstract class HuffmanTreeElement(val freq:Int)

    case class Leaf(override val freq:Int, symbol:String) extends HuffmanTreeElement(freq)
    case class Node(override val freq:Int, left:HuffmanTreeElement, right:HuffmanTreeElement) extends HuffmanTreeElement(freq)

    implicit object ord extends Ordering[HuffmanTreeElement] {
      override def compare(x: HuffmanTreeElement, y: HuffmanTreeElement): Int = y.freq - x.freq
    }

    def treeToList(tree:HuffmanTreeElement):List[(String, String)] = {

      @tailrec
      def treeToListR(tree:HuffmanTreeElement, prefix:String, soFar:List[(String, String)],
                      rest:List[(HuffmanTreeElement, String)]):List[(String, String)] =
        tree match {
          case Leaf(_, symbol) if rest.isEmpty => (symbol, prefix) :: soFar
          case Leaf(_, symbol) => treeToListR(rest.head._1, rest.head._2, (symbol, prefix) :: soFar, rest.tail)
          case Node(_, left, right) => treeToListR(left, prefix + "0", soFar, (right, prefix + "1") :: rest)
        }

      treeToListR(tree, "", List(), List()).reverse
    }

    val pq = new mutable.PriorityQueue[HuffmanTreeElement]
    pq.enqueue(symbols map (x => Leaf(x._2, x._1)): _*)

    while (pq.length > 1) {
      val a = pq.dequeue()
      val b = pq.dequeue()

      pq.enqueue(Node(a.freq + b.freq, a, b))
    }

    if (pq.length == 1) {
      treeToList(pq.dequeue())
    } else {
      List()
    }
  }
}
