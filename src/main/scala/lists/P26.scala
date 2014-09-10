package lists

object P26 {
  def combinations[T](k: Int, lst: List[T]):List[List[T]] =
    if (k < 1) {
      List(List())
    }
    else {
      lst.tails.flatMap(t => if (t.nonEmpty) combinations(k - 1, t.tail).map(t2 => t.head :: t2) else List()).toList
    }

}
