object P27 {
  case class ExtractWithRest[T](elements:List[T], rest:List[T])

  def combinationsWithRests[T](k: Int, lst: List[T]):List[ExtractWithRest[T]] =
    if (k < 1)
      List(ExtractWithRest(List(), lst))
    else {
      val splits = for(i <- 0 to lst.length - 1) yield lst.splitAt(i)
      splits.flatMap({
        case (before, xs) => if (xs.isEmpty) List() else combinationsWithRests(k - 1, xs.tail) map {
          case ExtractWithRest(elems, rests) => ExtractWithRest(xs.head :: elems, before ::: rests)
        }
      }).toList
    }

  def group3[T](lst: List[T]):List[List[List[T]]] =
    if (lst.length != 9)
      List()
    else
      combinationsWithRests(2, lst) flatMap (e => combinationsWithRests(3, e.rest) map (e2 => List(e.elements, e2.elements, e2.rest)))

  def group[T](parts:List[Int], lst: List[T]):List[List[List[T]]] =
    if (lst.length != parts.sum)
      List()
    else
      parts match {
        case p::ps => combinationsWithRests(p, lst) flatMap ( e => group(ps, e.rest) map (e.elements :: _))
        case _ => List(List())
      }

}
