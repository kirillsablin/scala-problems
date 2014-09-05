object P10 {
  import P09.pack

  def encode[T](lst: List[T]):List[(Int, T)] = pack(lst) map (elems => (elems.length, elems.head))
}
