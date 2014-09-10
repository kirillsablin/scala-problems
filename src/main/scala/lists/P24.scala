package lists

object P24 {
  import P23.randomSelect
  import P22.range

  def lotto(n: Int, bound: Int): List[Int] = randomSelect(6, range(1, n + 1))
}
