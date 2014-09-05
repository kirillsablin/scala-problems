object P14 {
  def duplicate[T](lst: List[T]):List[T] = lst flatMap (x => List(x, x))
}
