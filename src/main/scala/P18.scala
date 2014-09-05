object P18 {
  def slice[T](from: Int, to: Int, lst: List[T]):List[T] = {
    def sliceR(from: Int, to: Int, rest: List[T], soFar:List[T]):List[T] = rest match {
      case x::xs if from > 0 => sliceR(from - 1, to - 1, xs, soFar)
      case x::xs if to > 0 => sliceR(0, to - 1, xs, x::soFar)
      case _ => soFar.reverse
    }

    sliceR(from, to, lst, List())
  }
}
