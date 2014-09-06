object P20 {
  def removeAt[T](n: Int, lst: List[T]):(List[T], T) = {
    if (n < 0) throw new NoSuchElementException

    val (before, elementAndAfter) = lst.splitAt(n)

    if (elementAndAfter == Nil) throw new NoSuchElementException

    (before ::: elementAndAfter.tail, elementAndAfter.head)
  }

}
