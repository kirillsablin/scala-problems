object P23 {
  import P20.removeAt

  def randomSelect[T](n: Int, lst: List[T]):List[T] =
    if (n == 0 || lst.isEmpty)
      List()
    else {
      val (rest, elem) = removeAt((math.random * lst.length).toInt, lst)
      elem::randomSelect(n-1, rest)
    }
}
