package lists

object P12 {
  def decode[T](lst: List[(Int,T)]):List[T] = lst flatMap {
    case (number, elem) => for (_  <- 1 to number) yield elem
  }
}
