object P16 {
  def drop[T](n: Int, lst: List[T]):List[T] = {
    def dropR(counter:Int, rest:List[T], soFar:List[T]):List[T] = rest match {
      case _::xs if counter == n => dropR(1, xs, soFar)
      case x::xs => dropR(counter + 1, xs, x::soFar)
      case _ => soFar
    }

    dropR(1, lst, List()).reverse
  }
}
