package logic

object S99Logic {
  def and(a: Boolean, b: Boolean):Boolean = a && b
  def or(a: Boolean, b: Boolean):Boolean = a || b
  def nand(a: Boolean, b: Boolean) = !(a && b)

  private val booleans = List(true, false)

  def table2(f: (Boolean, Boolean) => Boolean):List[((Boolean, Boolean), Boolean)] =
    for (a <- booleans; b <- booleans) yield ((a, b), f(a, b))
}
