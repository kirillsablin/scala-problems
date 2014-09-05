object P07 {
  def flatten(lst: List[Any]):List[Any] = lst.flatMap {
    case a: List[Any] => flatten(a)
    case a => List(a)
  }
}
