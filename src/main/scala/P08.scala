object P08 {
  def compressFunctional[T](lst: List[T]):List[T] = lst.foldRight(List[T]())((element, prev) =>
    if (prev.isEmpty || prev.head != element)
      element::prev
    else
      prev)

  def compress[T](lst: List[T]):List[T] = {
    def compressR(src:List[T], soFar:List[T]):List[T] = src match {
      case x::xs => compressR(xs.dropWhile( _ == x), x::soFar)
      case _ => soFar
    }

    compressR(lst, List()).reverse
  }

}
