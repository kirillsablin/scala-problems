package lists

import scala.annotation.tailrec

object P02 {
  @tailrec
  def penultimate[T](lst: List[T]):T = lst match {
    case x::_::Nil => x
    case _::xs => penultimate(xs)
    case _ => throw new NoSuchElementException
  }

  def penultimateNativeBased[T](lst: List[T]):T =
    if (lst.isEmpty)
      throw new NoSuchElementException
    else
      lst.init.last

}
