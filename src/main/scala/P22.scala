object P22 {
  def range(from: Int, to: Int):List[Int] = {
    def rangeR(n:Int, soFar:List[Int]):List[Int] =
      if (n > to)
        soFar.reverse
      else
        rangeR(n+1, n::soFar)

    rangeR(from, List())
  }
}
