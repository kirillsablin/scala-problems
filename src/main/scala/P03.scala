import scala.annotation.tailrec

object P03 {

  @tailrec
  def nth[T](n: Int, lst: List[T]):T = lst match {
    case x::_ if n == 0 => x
    case _::xs => nth(n-1, xs)
    case _ => throw new NoSuchElementException
  }

  def nativeNth[T](n: Int, lst: List[T]):T = lst.drop(n).head

}
