package lists

object P22 {
  def range(from: Int, to: Int):List[Int] = {
    def rangeR(n:Int, soFar:List[Int]):List[Int] =
      if (from > n)
        soFar
      else
        rangeR(n-1, n::soFar)

    rangeR(to, List())
  }
}
