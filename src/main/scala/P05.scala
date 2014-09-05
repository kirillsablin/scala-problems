import scala.annotation.tailrec

object P05 {
  def reverse[T](lst: List[T]):List[T] = {
    @tailrec
    def reverseTailRecursive(src: List[T], accu: List[T]): List[T] = src match {
      case x :: xs => reverseTailRecursive(xs, x :: accu)
      case Nil => accu
    }
    reverseTailRecursive(lst, List())
  }

  def functionalReverse[T](lst: List[T]):List[T] = lst.foldLeft(List[T]())((soFar, elem) =>  elem::soFar)
}
