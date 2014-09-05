import java.util.NoSuchElementException

import scala.annotation.tailrec

object P01 {
  @tailrec
  def last[T](lst: List[T]):T = lst match {
    case x::Nil => x
    case _::xs => last(xs)
    case _ => throw new NoSuchElementException

  }

  def internalLast[T](lst: List[T]):T = lst.last

  def functionalLast[T](lst: List[T]):T = lst.map(Some(_) ).foldLeft[Option[T]](None)((prev:Option[T], current:Option[T]) => current).get


}
