object P21 {
  def insertAt[T](elem: T, position: Int, lst: List[T]):List[T] = {
    val (before, after) = lst.splitAt(position)

    before ::: (elem :: after)
  }
}
