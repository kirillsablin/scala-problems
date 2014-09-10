package lists

object P15 {
  def duplicateN[T](n: Int, lst: List[T]):List[T] = lst.flatMap( x => List.fill(n)(x))
}
