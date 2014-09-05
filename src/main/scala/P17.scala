object P17 {
  def split[T](n: Int, lst: List[T]):(List[T], List[T]) = (lst.take(n), lst.drop(n))

}
