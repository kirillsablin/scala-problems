package lists

object P19 {
  def rotate[T](n: Int, lst: List[T]):List[T] = {
    val mod = n % lst.length
    val bound = if (mod >= 0) mod else lst.length + mod

    val (h, t) = lst.splitAt(bound)
    t ::: h
  }
}
