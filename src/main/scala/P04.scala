import scala.annotation.tailrec

object P04 {
  def len(lst: List[_]): Int = {
    @tailrec
    def lenRec(lst: List[_], soFar: Int): Int = lst match {
      case _ :: xs => lenRec(xs, soFar + 1)
      case _ => soFar
    }

    lenRec(lst, 0)
  }

  def funcLen(lst: List[_]): Int =
    lst.foldLeft(0)((soFar, _) => soFar + 1)
}