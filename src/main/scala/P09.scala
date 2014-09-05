object P09 {

  def pack[T](lst: List[T]):List[List[T]] = {
    def packR(remain:List[T], soFar:List[List[T]]):List[List[T]]  = remain match {
      case x::xs => packR(xs.dropWhile(_ == x), (x :: xs.takeWhile(_ == x)) :: soFar)
      case _ => soFar

    }

    packR(lst, List()).reverse
  }
}
